<?php
require_once dirname(__FILE__).DIRECTORY_SEPARATOR.'sockettransportmpt.class.php';
class SmppClientMPT
{
	// SMPPMPT bind parameters
	public static $system_type="WWW";
	public static $interface_version=0x34;
	public static $addr_ton=0;
	public static $addr_npi=0;
	public static $address_range="";
	
	// ESME transmitter parameters
	public static $sms_service_type="";
	public static $sms_esm_class=0x00;
	public static $sms_protocol_id=0x00;
	public static $sms_priority_flag=0x00;
	public static $sms_registered_delivery_flag=0x00;
	public static $sms_replace_if_present_flag=0x00;
	public static $sms_sm_default_msg_id=0x00;
	
	/**
	 * SMPPMPT v3.4 says octect string are "not necessarily NULL terminated".
	 * Switch to toggle this feature
	 * @var boolean
	 */
	public static $sms_null_terminate_octetstrings=true;
	
	/**
	 * Use sar_msg_ref_num and sar_total_segments with 16 bit tags
	 * @var integer
	 */
	const CSMS_16BIT_TAGS = 0;
	
	/**
	 * Use message payload for CSMS
	 * @var integer
	 */
	const CSMS_PAYLOAD = 1;
	
	/**
	 * Embed a UDH in the message with 8-bit reference.
	 * @var integer
	 */
	const CSMS_8BIT_UDH = 2;
	
	public static $csms_method = SmppClientMPT::CSMS_16BIT_TAGS;
	
	public $debug;
	
	protected $pdu_queue;
	
	protected $transport;
	protected $debugHandler;
	
	// Used for reconnect
	protected $mode;
	private $login;
	private $pass;
	
	protected $sequence_number;
	protected $sar_msg_ref_num;

	/**
	 * Construct the SMPPMPT class
	 * 
	 * @param SocketTransportMPT $transport
	 * @param string $debugHandler
	 */
	public function __construct(SocketTransportMPT $transport,$debugHandler=null)
	{
		// Internal parameters
		$this->sequence_number=1;
		$this->debug=false;
		$this->pdu_queue=array();
		
		$this->transport = $transport;
		$this->debugHandler = $debugHandler ? $debugHandler : 'error_log';
		$this->mode = null;
	}
	
	/**
	 * Binds the receiver. One object can be bound only as receiver or only as trancmitter.
	 * @param string $login - ESME system_id
	 * @param string $pass - ESME password
	 * @throws SmppExceptionMPT
	 */
	public function bindReceiver($login, $pass)
	{
		if (!$this->transport->isOpen()) return false;
		if($this->debug) call_user_func($this->debugHandler, 'Binding receiver...');
		 
		$response = $this->_bind($login, $pass, SMPPMPT::BIND_RECEIVER);
		
		if($this->debug) call_user_func($this->debugHandler, "Binding status  : ".$response->status);
		$this->mode = 'receiver';
		$this->login = $login;
		$this->pass = $pass;
	}
	
	/**
	 * Binds the transmitter. One object can be bound only as receiver or only as trancmitter.
	 * @param string $login - ESME system_id
	 * @param string $pass - ESME password
	 * @throws SmppExceptionMPT
	 */
	public function bindTransmitter($login, $pass)
	{
		if (!$this->transport->isOpen()) return false;
		if($this->debug) call_user_func($this->debugHandler, 'Binding transmitter...');
		
		$response = $this->_bind($login, $pass, SMPPMPT::BIND_TRANSMITTER);
		
		if($this->debug) call_user_func($this->debugHandler, "Binding status  : ".$response->status);
		$this->mode = 'transmitter';
		$this->login = $login;
		$this->pass = $pass;
	}

	// Added by Lwin on 25 June 2018
	/**
	 * Binds the transceiver. One object can be bound as both transmitter and receiver in one connection.
	 * @param string $login - ESME system_id
	 * @param string $pass - ESME password
	 * @throws SmppExceptionMPT
	 */
	/*
	public function bindTransceiver($login, $pass)
        {
                if (!$this->transport->isOpen()) return false;
                if($this->debug) call_user_func($this->debugHandler, 'Binding transceiver...');

                $response = $this->_bind($login, $pass, SMPPMPT::BIND_TRANSCEIVER);

                if($this->debug) call_user_func($this->debugHandler, "Binding status  : ".$response->status);
                $this->mode = 'transceiver';
                $this->login = $login;
                $this->pass = $pass;
		return true;
        }
	*/
	// End addition
	
	/**
	 * Closes the session on the SMSC server.
	 */
	public function close()
	{
		if (!$this->transport->isOpen()) return;
		if($this->debug) call_user_func($this->debugHandler, 'Unbinding...');
		
		$response=$this->sendCommand(SMPPMPT::UNBIND,"");
		
		if($this->debug) call_user_func($this->debugHandler, "Unbind status   : ".$response->status);
		$this->transport->close();
	}
	
	/**
	 * Parse a timestring as formatted by SMPPMPT v3.4 section 7.1.
	 * Returns an unix timestamp if $newDates is false or DateTime/DateInterval is missing,
	 * otherwise an object of either DateTime or DateInterval is returned.
	 * 
	 * @param string $input
	 * @param boolean $newDates
	 * @return mixed
	 */
	public function parseSmppTime($input, $newDates=true)
	{
		// Check for support for new date classes
		if (!class_exists('DateTime') || !class_exists('DateInterval')) $newDates = false;
		
		$numMatch = preg_match('/^(\\d{2})(\\d{2})(\\d{2})(\\d{2})(\\d{2})(\\d{2})(\\d{1})(\\d{2})([R+-])$/',$input,$matches);
		if (!$numMatch) return null;
		list($whole, $y, $m, $d, $h, $i, $s, $t, $n, $p) = $matches;
		
		// Use strtotime to convert relative time into a unix timestamp
		if ($p == 'R') {
			if ($newDates) {
				$spec = "P";
				if ($y) $spec .= $y.'Y';
				if ($m) $spec .= $m.'M';
				if ($d) $spec .= $d.'D';
				if ($h || $i || $s) $spec .= 'T';
				if ($h) $spec .= $h.'H';
				if ($i) $spec .= $i.'M';
				if ($s) $spec .= $s.'S';
				return new DateInterval($spec);
			} else {
				return strtotime("+$y year +$m month +$d day +$h hour +$i minute $s +second");
			}
		} else {
			$offsetHours = floor($n/4);
			$offsetMinutes = ($n % 4)*15;
			$time = sprintf("20%02s-%02s-%02sT%02s:%02s:%02s%s%02s:%02s",$y,$m,$d,$h,$i,$s,$p,$offsetHours,$offsetMinutes); // Not Y3K safe
			if ($newDates) {
				return new DateTime($time);
			} else {
				return strtotime($time);
			}
		}
	}
	
	/**
	 * Query the SMSC about current state/status of a previous sent SMS.
	 * You must specify the SMSC assigned message id and source of the sent SMS.
	 * Returns an associative array with elements: message_id, final_date, message_state and error_code.
	 * 	message_state would be one of the SMPPMPT::STATE_* constants. (SMPPMPT v3.4 section 5.2.28)
	 * 	error_code depends on the telco network, so could be anything.
	 * 
	 * @param string $messageid
	 * @param SmppAddressMPT $source
	 * @return array
	 */
	public function queryStatus($messageid,SmppAddressMPT $source)
	{
		$pduBody = pack('a'.(strlen($messageid)+1).'cca'.(strlen($source->value)+1),$messageid,$source->ton,$source->npi,$source->value);
		$reply = $this->sendCommand(SMPPMPT::QUERY_SM, $pduBody);
		if (!$reply || $reply->status != SMPPMPT::ESME_ROK) return null;
		
		// Parse reply
		$posId = strpos($reply->body,"\0",0);
		$posDate = strpos($reply->body,"\0",$posId+1);
		$data = array();
		$data['message_id'] = substr($reply->body,0,$posId);
		$data['final_date'] = substr($reply->body,$posId,$posDate-$posId);
		$data['final_date'] = $data['final_date'] ? $this->parseSmppTime(trim($data['final_date'])) : null;
		$status = unpack("cmessage_state/cerror_code",substr($reply->body,$posDate+1));
		return array_merge($data,$status);
	}
	
	/**
	 * Read one SMS from SMSC. Can be executed only after bindReceiver() call. 
	 * This method bloks. Method returns on socket timeout or enquire_link signal from SMSC.
	 * @return sms associative array or false when reading failed or no more sms.
	 */
	public function readSMS()
	{
		$command_id=SMPPMPT::DELIVER_SM;
		// Check the queue
		$ql = count($this->pdu_queue);
		for($i=0;$i<$ql;$i++) {
			$pdu=$this->pdu_queue[$i];
			if($pdu->id==$command_id) {
				//remove response
				array_splice($this->pdu_queue, $i, 1);
				return $this->parseSMS($pdu);
			}
		}
		// Read pdu
		do{
			$pdu = $this->readPDU();
			if ($pdu === false) return false; // TSocket v. 0.6.0+ returns false on timeout
			//check for enquire link command
			if($pdu->id==SMPPMPT::ENQUIRE_LINK) {
				$response = new SmppPduMPT(SMPPMPT::ENQUIRE_LINK_RESP, SMPPMPT::ESME_ROK, $pdu->sequence, "\x00");
				$this->sendPDU($response);
			} else if ($pdu->id!=$command_id) { // if this is not the correct PDU add to queue
				array_push($this->pdu_queue, $pdu);
			}
		} while($pdu && $pdu->id!=$command_id);
		
		if($pdu) return $this->parseSMS($pdu);
		return false;
	}
	
	/**
	 * Send one SMS to SMSC. Can be executed only after bindTransmitter() call.
	 * $message is always in octets regardless of the data encoding.
	 * For correct handling of Concatenated SMS, message must be encoded with GSM 03.38 (data_coding 0x00) or UCS-2BE (0x08).
	 * Concatenated SMS'es uses 16-bit reference numbers, which gives 152 GSM 03.38 chars or 66 UCS-2BE chars per CSMS.
	 * If we are using 8-bit ref numbers in the UDH for CSMS it's 153 GSM 03.38 chars
	 * 
	 * @param SmppAddressMPT $from
	 * @param SmppAddressMPT $to
	 * @param string $message
	 * @param array $tags (optional)
	 * @param integer $dataCoding (optional)
	 * @param integer $priority (optional)
	 * @param string $scheduleDeliveryTime (optional)
	 * @param string $validityPeriod (optional)
	 * @return string message id
	 */
	public function sendSMS(SmppAddressMPT $from, SmppAddressMPT $to, $message, $tags=null, $dataCoding=SMPPMPT::DATA_CODING_DEFAULT, $priority=0x00, $scheduleDeliveryTime=null, $validityPeriod=null)
	{
		$msg_length = strlen($message);
		
		if ($msg_length>160 && $dataCoding != SMPPMPT::DATA_CODING_UCS2 && $dataCoding != SMPPMPT::DATA_CODING_DEFAULT) return false;
		
		switch ($dataCoding) {
			case SMPPMPT::DATA_CODING_UCS2:
				$singleSmsOctetLimit = 140; // in octets, 70 UCS-2 chars
				$csmsSplit = 132; // There are 133 octets available, but this would split the UCS the middle so use 132 instead
				break;
			case SMPPMPT::DATA_CODING_DEFAULT:
				$singleSmsOctetLimit = 160; // we send data in octets, but GSM 03.38 will be packed in septets (7-bit) by SMSC.
				$csmsSplit = (self::$csms_method == SmppClientMPT::CSMS_8BIT_UDH) ? 153 : 152; // send 152/153 chars in each SMS (SMSC will format data)
				break;
			default:
				$singleSmsOctetLimit = 254; // From SMPPMPT standard
				break;
		}
		
		// Figure out if we need to do CSMS, since it will affect our PDU
		if ($msg_length > $singleSmsOctetLimit) {
			$doCsms = true;
			if (self::$csms_method != SmppClientMPT::CSMS_PAYLOAD) {
				$parts = $this->splitMessageString($message, $csmsSplit, $dataCoding);
				$short_message = reset($parts);
				$csmsReference = $this->getCsmsReference();
			}
		} else {
			$short_message = $message;
			$doCsms = false;
		}
		
		// Deal with CSMS
		if ($doCsms) {
			if (self::$csms_method == SmppClientMPT::CSMS_PAYLOAD) {
				$payload = new SmppTagMPT(SmppTagMPT::MESSAGE_PAYLOAD, $message, $msg_length);
				return $this->submit_sm($from, $to, null, (empty($tags) ? array($payload) : array_merge($tags,$payload)), $dataCoding, $priority, $scheduleDeliveryTime, $validityPeriod);
			} else if (self::$csms_method == SmppClientMPT::CSMS_8BIT_UDH) {
				$seqnum = 1;
				foreach ($parts as $part) {
					$udh = pack('cccccc',5,0,3,substr($csmsReference,1,1),count($parts),$seqnum);
					$res = $this->submit_sm($from, $to, $udh.$part, $tags, $dataCoding, $priority, $scheduleDeliveryTime, $validityPeriod, (SmppClientMPT::$sms_esm_class|0x40));
					$seqnum++;
				}
				return $res;
			} else {
				$sar_msg_ref_num = new SmppTagMPT(SmppTagMPT::SAR_MSG_REF_NUM, $csmsReference, 2, 'n');
				$sar_total_segments = new SmppTagMPT(SmppTagMPT::SAR_TOTAL_SEGMENTS, count($parts), 1, 'c');
				$seqnum = 1;
				foreach ($parts as $part) {
					$sartags = array($sar_msg_ref_num, $sar_total_segments, new SmppTagMPT(SmppTagMPT::SAR_SEGMENT_SEQNUM, $seqnum, 1, 'c'));
					$res = $this->submit_sm($from, $to, $part, (empty($tags) ? $sartags : array_merge($tags,$sartags)), $dataCoding, $priority, $scheduleDeliveryTime, $validityPeriod);
					$seqnum++;
				}
				return $res;
			}
		}
		
		return $this->submit_sm($from, $to, $short_message, $tags, $dataCoding);
	}
	
	/**
	 * Perform the actual submit_sm call to send SMS.
	 * Implemented as a protected method to allow automatic sms concatenation.
	 * Tags must be an array of already packed and encoded TLV-params.
	 * 
	 * @param SmppAddressMPT $source
	 * @param SmppAddressMPT $destination
	 * @param string $short_message
	 * @param array $tags
	 * @param integer $dataCoding
	 * @param integer $priority
	 * @param string $scheduleDeliveryTime
	 * @param string $validityPeriod
	 * @param string $esmClass
	 * @return string message id
	 */
	protected function submit_sm(SmppAddressMPT $source, SmppAddressMPT $destination, $short_message=null, $tags=null, $dataCoding=SMPPMPT::DATA_CODING_DEFAULT, $priority=0x00, $scheduleDeliveryTime=null, $validityPeriod=null, $esmClass=null)
	{
		if (is_null($esmClass)) $esmClass = self::$sms_esm_class;
		
		// Construct PDU with mandatory fields
		$pdu = pack('a1cca'.(strlen($source->value)+1).'cca'.(strlen($destination->value)+1).'ccc'.($scheduleDeliveryTime ? 'a16x' : 'a1').($validityPeriod ? 'a16x' : 'a1').'ccccca'.(strlen($short_message)+(self::$sms_null_terminate_octetstrings ? 1 : 0)),
			self::$sms_service_type,
			$source->ton,
			$source->npi,
			$source->value,
			$destination->ton,
			$destination->npi,
			$destination->value,
			$esmClass,
			self::$sms_protocol_id,
			$priority,
			$scheduleDeliveryTime,
			$validityPeriod,
			self::$sms_registered_delivery_flag,
			self::$sms_replace_if_present_flag,
			$dataCoding,
			self::$sms_sm_default_msg_id,
			strlen($short_message),//sm_length
			$short_message//short_message
		);
		
		// Add any tags
		if (!empty($tags)) {
			foreach ($tags as $tag) {
				$pdu .= $tag->getBinary();
			}
		}
		
		$response=$this->sendCommand(SMPPMPT::SUBMIT_SM,$pdu);
		$body = unpack("a*msgid",$response->body);
		return $body['msgid'];
	}
	
	/**
	 * Get a CSMS reference number for sar_msg_ref_num.
	 * Initializes with a random value, and then returns the number in sequence with each call.
	 */
	protected function getCsmsReference()
	{
		$limit = (SmppClientMPT::$csms_method == SmppClientMPT::CSMS_8BIT_UDH) ? 255 : 65535;
		if (!isset($this->sar_msg_ref_num)) $this->sar_msg_ref_num = mt_rand(0,$limit);
		$this->sar_msg_ref_num++;
		if ($this->sar_msg_ref_num>$limit) $this->sar_msg_ref_num = 0;
		return $this->sar_msg_ref_num;
	}
	
	
	/**
	 * Split a message into multiple parts, taking the encoding into account.
	 * A character represented by an GSM 03.38 escape-sequence shall not be split in the middle.
	 * Uses str_split if at all possible, and will examine all split points for escape chars if it's required.
	 * 
	 * @param string $message
	 * @param integer $split
	 * @param integer $dataCoding (optional)
	 */
	protected function splitMessageString($message, $split, $dataCoding=SMPPMPT::DATA_CODING_DEFAULT)
	{
		switch ($dataCoding) {
			case SMPPMPT::DATA_CODING_DEFAULT:
				$msg_length = strlen($message);
				// Do we need to do php based split?
				$numParts = floor($msg_length / $split);
				if ($msg_length % $split == 0) $numParts--;
				$slowSplit = false;
				
				for($i=1;$i<=$numParts;$i++) {
					if ($message[$i*$split-1] == "\x1B") {
						$slowSplit = true;
						break;
					};
				}
				if (!$slowSplit) return str_split($message,$split);
				
				// Split the message char-by-char
				$parts = array();
				$part = null;
				$n = 0;
				for($i=0;$i<$msg_length;$i++) {
					$c = $message[$i];
					// reset on $split or if last char is a GSM 03.38 escape char
					if ($n==$split || ($n==($split-1) && $c=="\x1B")) {  
						$parts[] = $part;
						$n = 0;
						$part = null;
					}
					$part .= $c;
				}
				$parts[] = $part;
				return $parts;
			case SMPPMPT::DATA_CODING_UCS2: // UCS2-BE can just use str_split since we send 132 octets per message, which gives a fine split using UCS2
			default:
				return str_split($message,$split);
		}
	}

	/**
	 * Binds the socket and opens the session on SMSC
	 * @param string $login - ESME system_id
	 * @param string $port - ESME password
	 * @return SmppPduMPT
	 */
	protected function _bind($login, $pass, $command_id)
	{
		// Make PDU body
		$pduBody = pack(
			'a'.(strlen($login)+1).
			'a'.(strlen($pass)+1).
			'a'.(strlen(self::$system_type)+1).
			'CCCa'.(strlen(self::$address_range)+1),
			$login, $pass, self::$system_type,
			self::$interface_version, self::$addr_ton,
			self::$addr_npi, self::$address_range
		);
		
		$response=$this->sendCommand($command_id,$pduBody);
		if ($response->status != SMPPMPT::ESME_ROK) throw new SmppExceptionMPT(SMPPMPT::getStatusMessage($response->status), $response->status);
		
		return $response;
	}

	/**
	 * Parse received PDU from SMSC.
	 * @param SmppPduMPT $pdu - received PDU from SMSC.
	 * @return parsed PDU as array.
	 */
	protected function parseSMS(SmppPduMPT $pdu)
	{
		// Check command id
		if($pdu->id != SMPPMPT::DELIVER_SM) throw new InvalidArgumentException('PDU is not an received SMS');

		// Unpack PDU
		$ar=unpack("C*",$pdu->body);
		
		// Read mandatory params
		$service_type = $this->getString($ar,6,true);
		
		$source_addr_ton = next($ar);
		$source_addr_npi = next($ar);
		$source_addr = $this->getString($ar,21);
		$source = new SmppAddressMPT($source_addr,$source_addr_ton,$source_addr_npi);
		
		$dest_addr_ton = next($ar);
		$dest_addr_npi = next($ar);
		$destination_addr = $this->getString($ar,21);
		$destination = new SmppAddressMPT($destination_addr,$dest_addr_ton,$dest_addr_npi);
		
		$esmClass = next($ar);
		$protocolId = next($ar);
		$priorityFlag = next($ar);
		next($ar); // schedule_delivery_time
		next($ar); // validity_period 
		$registeredDelivery = next($ar);
		next($ar); // replace_if_present_flag 
		$dataCoding = next($ar);
		next($ar); // sm_default_msg_id 
		$sm_length = next($ar);
		$message = $this->getString($ar,$sm_length);
		
		// Check for optional params, and parse them
		if (current($ar) !== false) {
			$tags = array();
			do {
				$tag = $this->parseTag($ar);
				if ($tag !== false) $tags[] = $tag;
			} while (current($ar) !== false);
		} else {
			$tags = null;
		}
		
		if (($esmClass & SMPPMPT::ESM_DELIVER_SMSC_RECEIPT) != 0) {
			$sms = new SmppDeliveryReceiptMPT($pdu->id, $pdu->status, $pdu->sequence, $pdu->body, $service_type, $source, $destination, $esmClass, $protocolId, $priorityFlag, $registeredDelivery, $dataCoding, $message, $tags);
			$sms->parseDeliveryReceipt();
		} else {
			$sms = new SmppSmsMPT($pdu->id, $pdu->status, $pdu->sequence, $pdu->body, $service_type, $source, $destination, $esmClass, $protocolId, $priorityFlag, $registeredDelivery, $dataCoding, $message, $tags);
		}
		
		if($this->debug) call_user_func($this->debugHandler, "Received sms:\n".print_r($sms,true));
		
		// Send response of recieving sms
		$response = new SmppPduMPT(SMPPMPT::DELIVER_SM_RESP, SMPPMPT::ESME_ROK, $pdu->sequence, "\x00");
		$this->sendPDU($response);
		return $sms;
	}
	
	/**
	 * Send the enquire link command.
	 * @return SmppPduMPT
	 */
	public function enquireLink()
	{
		$response = $this->sendCommand(SMPPMPT::ENQUIRE_LINK, null);
		return $response;
	}
	
	/**
	 * Respond to any enquire link we might have waiting.
	 * If will check the queue first and respond to any enquire links we have there.
	 * Then it will move on to the transport, and if the first PDU is enquire link respond, 
	 * otherwise add it to the queue and return.
	 * 
	 */
	public function respondEnquireLink()
	{
		// Check the queue first
		$ql = count($this->pdu_queue);
		for($i=0;$i<$ql;$i++) {
			$pdu=$this->pdu_queue[$i];
			if($pdu->id==SMPPMPT::ENQUIRE_LINK) {
				//remove response
				array_splice($this->pdu_queue, $i, 1);
				$this->sendPDU(new SmppPduMPT(SMPPMPT::ENQUIRE_LINK_RESP, SMPPMPT::ESME_ROK, $pdu->sequence, "\x00"));
			}
		}
		
		// Check the transport for data
		if ($this->transport->hasData()) {
			$pdu = $this->readPDU();
			if($pdu->id==SMPPMPT::ENQUIRE_LINK) {
				$this->sendPDU(new SmppPduMPT(SMPPMPT::ENQUIRE_LINK_RESP, SMPPMPT::ESME_ROK, $pdu->sequence, "\x00"));
			} else if ($pdu) {
				array_push($this->pdu_queue, $pdu);
			}
		}
	}
	
	/**
	 * Reconnect to SMSC.
	 * This is mostly to deal with the situation were we run out of sequence numbers
	 */
	protected function reconnect()
	{
		$this->close();
		sleep(1);
		$this->transport->open();
		$this->sequence_number = 1;
		
		if ($this->mode == 'receiver') {
			$this->bindReceiver($this->login, $this->pass);
		} else {
			$this->bindTransmitter($this->login, $this->pass);
		}
	}
	
	/**
	 * Sends the PDU command to the SMSC and waits for response.
	 * @param integer $id - command ID
	 * @param string $pduBody - PDU body
	 * @return SmppPduMPT
	 */
	protected function sendCommand($id, $pduBody)
	{
		if (!$this->transport->isOpen()) return false;
		$pdu = new SmppPduMPT($id, 0, $this->sequence_number, $pduBody);
		$this->sendPDU($pdu);
		$response=$this->readPDU_resp($this->sequence_number, $pdu->id);
		if ($response === false) throw new SmppExceptionMPT('Failed to read reply to command: 0x'.dechex($id));
		
		if ($response->status != SMPPMPT::ESME_ROK) throw new SmppExceptionMPT(SMPPMPT::getStatusMessage($response->status), $response->status);
		
		$this->sequence_number++;
		
		// Reached max sequence number, spec does not state what happens now, so we re-connect
		if ($this->sequence_number >= 0x7FFFFFFF) {
			$this->reconnect();
		}
		
		return $response;
	}
	
	/**
	 * Prepares and sends PDU to SMSC.
	 * @param SmppPduMPT $pdu
	 */
	protected function sendPDU(SmppPduMPT $pdu)
	{
		$length=strlen($pdu->body) + 16;
		$header=pack("NNNN", $length, $pdu->id, $pdu->status, $pdu->sequence);
		if($this->debug) {
			call_user_func($this->debugHandler, "Send PDU         : $length bytes");
			call_user_func($this->debugHandler, ' '.chunk_split(bin2hex($header.$pdu->body),2," "));
			call_user_func($this->debugHandler, ' command_id      : 0x'.dechex($pdu->id));
			call_user_func($this->debugHandler, ' sequence number : '.$pdu->sequence);
		}
		$this->transport->write($header.$pdu->body,$length);
	}
	
	/**
	 * Waits for SMSC response on specific PDU.
	 * If a GENERIC_NACK with a matching sequence number, or null sequence is received instead it's also accepted.
	 * Some SMPPMPT servers, ie. logica returns GENERIC_NACK on errors.
	 * 
	 * @param integer $seq_number - PDU sequence number
	 * @param integer $command_id - PDU command ID
	 * @return SmppPduMPT
	 * @throws SmppExceptionMPT
	 */
	protected function readPDU_resp($seq_number, $command_id)
	{
		// Get response cmd id from command id
		$command_id=$command_id|SMPPMPT::GENERIC_NACK;
		
		// Check the queue first
		$ql = count($this->pdu_queue);
		for($i=0;$i<$ql;$i++) {
			$pdu=$this->pdu_queue[$i];
			if (
				($pdu->sequence == $seq_number && ($pdu->id == $command_id || $pdu->id == SMPPMPT::GENERIC_NACK)) || 
				($pdu->sequence == null && $pdu->id == SMPPMPT::GENERIC_NACK)
			) {
				// remove response pdu from queue
				array_splice($this->pdu_queue, $i, 1);
				return $pdu;
			}
		}
		
		// Read PDUs until the one we are looking for shows up, or a generic nack pdu with matching sequence or null sequence
		do{
			$pdu=$this->readPDU();
			if ($pdu) {
				if ($pdu->sequence == $seq_number && ($pdu->id == $command_id || $pdu->id == SMPPMPT::GENERIC_NACK)) return $pdu;
				if ($pdu->sequence == null && $pdu->id == SMPPMPT::GENERIC_NACK) return $pdu;
				array_push($this->pdu_queue, $pdu); // unknown PDU push to queue
			}
		} while($pdu);
		return false;
	}
	
	/**
	 * Reads incoming PDU from SMSC.
	 * @return SmppPduMPT
	 */
	protected function readPDU()
	{
		// Read PDU length
		$bufLength = $this->transport->read(4);
		if(!$bufLength) return false;
		extract(unpack("Nlength", $bufLength));
		
		// Read PDU headers
		$bufHeaders = $this->transport->read(12);
		if(!$bufHeaders)return false;
		extract(unpack("Ncommand_id/Ncommand_status/Nsequence_number", $bufHeaders));
		
		// Read PDU body
		if($length-16>0){
			$body=$this->transport->readAll($length-16);
			if(!$body) throw new RuntimeException('Could not read PDU body');
		} else {
			$body=null;
		}
		
		if($this->debug) {
			call_user_func($this->debugHandler, "Read PDU         : $length bytes");
			call_user_func($this->debugHandler, ' '.chunk_split(bin2hex($bufLength.$bufHeaders.$body),2," "));
			call_user_func($this->debugHandler, " command id      : 0x".dechex($command_id));
			call_user_func($this->debugHandler, " command status  : 0x".dechex($command_status)." ".SMPPMPT::getStatusMessage($command_status));
			call_user_func($this->debugHandler, ' sequence number : '.$sequence_number);
		}
		return new SmppPduMPT($command_id, $command_status, $sequence_number, $body);
	}
	
	/**
	 * Reads C style null padded string from the char array.
	 * Reads until $maxlen or null byte.
	 * 
	 * @param array $ar - input array
	 * @param integer $maxlen - maximum length to read.
	 * @param boolean $firstRead - is this the first bytes read from array? 
	 * @return read string.
	 */
	protected function getString(&$ar, $maxlen=255, $firstRead=false)
	{
		$s="";
		$i=0;
		do{
			$c = ($firstRead && $i==0) ? current($ar) : next($ar);
			if ($c != 0) $s .= chr($c);
			$i++;
		} while($i<$maxlen && $c !=0);
		return $s;
	}
	
	/**
	 * Read a specific number of octets from the char array.
	 * Does not stop at null byte
	 * 
	 * @param array $ar - input array
	 * @param intger $length
	 */
	protected function getOctets(&$ar,$length)
	{
		$s = "";
		for($i=0;$i<$length;$i++) {
			$c = next($ar);
			if ($c === false) return $s;
			$s .= chr($c);
		}
		return $s;
	}
	
	protected function parseTag(&$ar)
	{
		$unpackedData = unpack('nid/nlength',pack("C2C2",next($ar),next($ar),next($ar),next($ar)));
		if (!$unpackedData) throw new InvalidArgumentException('Could not read tag data');
		extract($unpackedData);
		
		// Sometimes SMSC return an extra null byte at the end
		if ($length==0 && $id == 0) {
			return false;	
		}
		
		$value = $this->getOctets($ar,$length);
		$tag = new SmppTagMPT($id, $value, $length);
		if ($this->debug) {
			call_user_func($this->debugHandler, "Parsed tag:");
			call_user_func($this->debugHandler, " id     :0x".dechex($tag->id));
			call_user_func($this->debugHandler, " length :".$tag->length);
			call_user_func($this->debugHandler, " value  :".chunk_split(bin2hex($tag->value),2," "));
		}
		return $tag;
	}
	
}

class SmppExceptionMPT extends RuntimeException
{
	
}


/**
 * Numerous constants for SMPPMPT v3.4
 * Based on specification at: http://www.smsforum.net/SMPPMPT_v3_4_Issue1_2.zip
 */ 

class SMPPMPT
{
	// Command ids - SMPPMPT v3.4 - 5.1.2.1 page 110-111
	const GENERIC_NACK = 0x80000000;
	const BIND_RECEIVER = 0x00000001;
	const BIND_RECEIVER_RESP = 0x80000001;
	const BIND_TRANSMITTER = 0x00000002;
	const BIND_TRANSMITTER_RESP = 0x80000002;
	const QUERY_SM = 0x00000003;
	const QUERY_SM_RESP = 0x80000003;
	const SUBMIT_SM = 0x00000004;
	const SUBMIT_SM_RESP = 0x80000004;
	const DELIVER_SM = 0x00000005;
	const DELIVER_SM_RESP = 0x80000005;
	const UNBIND = 0x00000006;
	const UNBIND_RESP = 0x80000006;
	const REPLACE_SM = 0x00000007;
	const REPLACE_SM_RESP = 0x80000007;
	const CANCEL_SM = 0x00000008;
	const CANCEL_SM_RESP = 0x80000008;
	const BIND_TRANSCEIVER = 0x00000009;
	const BIND_TRANSCEIVER_RESP = 0x80000009;
	const OUTBIND = 0x0000000B;
	const ENQUIRE_LINK = 0x00000015;
	const ENQUIRE_LINK_RESP = 0x80000015;
	
	//  Command status - SMPPMPT v3.4 - 5.1.3 page 112-114
	const ESME_ROK = 0x00000000; // No Error
	const ESME_RINVMSGLEN = 0x00000001; // Message Length is invalid
	const ESME_RINVCMDLEN = 0x00000002; // Command Length is invalid
	const ESME_RINVCMDID = 0x00000003; // Invalid Command ID
	const ESME_RINVBNDSTS = 0x00000004; // Incorrect BIND Status for given command
	const ESME_RALYBND = 0x00000005; // ESME Already in Bound State
	const ESME_RINVPRTFLG = 0x00000006; // Invalid Priority Flag
	const ESME_RINVREGDLVFLG = 0x00000007; // Invalid Registered Delivery Flag
	const ESME_RSYSERR = 0x00000008; // System Error
	const ESME_RINVSRCADR = 0x0000000A; // Invalid Source Address
	const ESME_RINVDSTADR = 0x0000000B; // Invalid Dest Addr
	const ESME_RINVMSGID = 0x0000000C; // Message ID is invalid
	const ESME_RBINDFAIL = 0x0000000D; // Bind Failed
	const ESME_RINVPASWD = 0x0000000E; // Invalid Password
	const ESME_RINVSYSID = 0x0000000F; // Invalid System ID
	const ESME_RCANCELFAIL = 0x00000011; // Cancel SM Failed
	const ESME_RREPLACEFAIL = 0x00000013; // Replace SM Failed
	const ESME_RMSGQFUL = 0x00000014; // Message Queue Full
	const ESME_RINVSERTYP = 0x00000015; // Invalid Service Type
	const ESME_RINVNUMDESTS = 0x00000033; // Invalid number of destinations
	const ESME_RINVDLNAME = 0x00000034; // Invalid Distribution List name
	const ESME_RINVDESTFLAG = 0x00000040; // Destination flag (submit_multi)
	const ESME_RINVSUBREP = 0x00000042; // Invalid ‘submit with replace’ request (i.e. submit_sm with replace_if_present_flag set)
	const ESME_RINVESMSUBMIT = 0x00000043; // Invalid esm_SUBMIT field data
	const ESME_RCNTSUBDL = 0x00000044; // Cannot Submit to Distribution List
	const ESME_RSUBMITFAIL = 0x00000045; // submit_sm or submit_multi failed
	const ESME_RINVSRCTON = 0x00000048; // Invalid Source address TON
	const ESME_RINVSRCNPI = 0x00000049; // Invalid Source address NPI
	const ESME_RINVDSTTON = 0x00000050; // Invalid Destination address TON
	const ESME_RINVDSTNPI = 0x00000051; // Invalid Destination address NPI
	const ESME_RINVSYSTYP = 0x00000053; // Invalid system_type field
	const ESME_RINVREPFLAG = 0x00000054; // Invalid replace_if_present flag
	const ESME_RINVNUMMSGS = 0x00000055; // Invalid number of messages
	const ESME_RTHROTTLED = 0x00000058; // Throttling error (ESME has exceeded allowed message limits)
	const ESME_RINVSCHED = 0x00000061; // Invalid Scheduled Delivery Time
	const ESME_RINVEXPIRY = 0x00000062; // Invalid message (Expiry time)
	const ESME_RINVDFTMSGID = 0x00000063; // Predefined Message Invalid or Not Found
	const ESME_RX_T_APPN = 0x00000064; // ESME Receiver Temporary App Error Code
	const ESME_RX_P_APPN = 0x00000065; // ESME Receiver Permanent App Error Code
	const ESME_RX_R_APPN = 0x00000066; // ESME Receiver Reject Message Error Code
	const ESME_RQUERYFAIL = 0x00000067; // query_sm request failed
	const ESME_RINVOPTPARSTREAM = 0x000000C0; // Error in the optional part of the PDU Body.
	const ESME_ROPTPARNOTALLWD = 0x000000C1; // Optional Parameter not allowed
	const ESME_RINVPARLEN = 0x000000C2; // Invalid Parameter Length.
	const ESME_RMISSINGOPTPARAM = 0x000000C3; // Expected Optional Parameter missing
	const ESME_RINVOPTPARAMVAL = 0x000000C4; // Invalid Optional Parameter Value
	const ESME_RDELIVERYFAILURE = 0x000000FE; // Delivery Failure (data_sm_resp)
	const ESME_RUNKNOWNERR = 0x000000FF; // Unknown Error
	
	// SMPPMPT v3.4 - 5.2.5 page 117	
	const TON_UNKNOWN = 0x00;
	const TON_INTERNATIONAL = 0x01;
	const TON_NATIONAL = 0x02;
	const TON_NETWORKSPECIFIC = 0x03;
	const TON_SUBSCRIBERNUMBER = 0x04;
	const TON_ALPHANUMERIC = 0x05;
	const TON_ABBREVIATED = 0x06;
	
	// SMPPMPT v3.4 - 5.2.6 page 118
	const NPI_UNKNOWN = 0x00;
	const NPI_E164 = 0x01;
	const NPI_DATA = 0x03;
	const NPI_TELEX = 0x04;
	const NPI_E212 = 0x06;
	const NPI_NATIONAL = 0x08;
	const NPI_PRIVATE = 0x09;
	const NPI_ERMES = 0x0a;
	const NPI_INTERNET = 0x0e;
	const NPI_WAPCLIENT = 0x12;
	
	// ESM bits 1-0 - SMPPMPT v3.4 - 5.2.12 page 121-122
	const ESM_SUBMIT_MODE_DATAGRAM = 0x01;
	const ESM_SUBMIT_MODE_FORWARD = 0x02;
	const ESM_SUBMIT_MODE_STOREANDFORWARD = 0x03;
	// ESM bits 5-2 
	const ESM_SUBMIT_BINARY = 0x04;
	const ESM_SUBMIT_TYPE_ESME_D_ACK = 0x08;
	const ESM_SUBMIT_TYPE_ESME_U_ACK = 0x10;
	const ESM_DELIVER_SMSC_RECEIPT = 0x04;
	const ESM_DELIVER_SME_ACK = 0x08;
	const ESM_DELIVER_U_ACK = 0x10;
	const ESM_DELIVER_CONV_ABORT = 0x18;
	const ESM_DELIVER_IDN = 0x20; // Intermediate delivery notification
	// ESM bits 7-6 
	const ESM_UHDI = 0x40;
	const ESM_REPLYPATH = 0x80;
	
	// SMPPMPT v3.4 - 5.2.17 page 124
	const REG_DELIVERY_NO = 0x00;
	const REG_DELIVERY_SMSC_BOTH = 0x01; // both success and failure
	const REG_DELIVERY_SMSC_FAILED = 0x02;
	const REG_DELIVERY_SME_D_ACK = 0x04;
	const REG_DELIVERY_SME_U_ACK = 0x08;
	const REG_DELIVERY_SME_BOTH = 0x10;
	const REG_DELIVERY_IDN = 0x16; // Intermediate notification
	
	// SMPPMPT v3.4 - 5.2.18 page 125
	const REPLACE_NO = 0x00;
	const REPLACE_YES = 0x01;
	
	// SMPPMPT v3.4 - 5.2.19 page 126
	const DATA_CODING_DEFAULT = 0;
	const DATA_CODING_IA5 = 1; // IA5 (CCITT T.50)/ASCII (ANSI X3.4)
	const DATA_CODING_BINARY_ALIAS = 2;
	const DATA_CODING_ISO8859_1 = 3; // Latin 1
	const DATA_CODING_BINARY = 4;
	const DATA_CODING_JIS = 5;
	const DATA_CODING_ISO8859_5 = 6; // Cyrllic
	const DATA_CODING_ISO8859_8 = 7; // Latin/Hebrew
	const DATA_CODING_UCS2 = 8; // UCS-2BE (Big Endian)
	const DATA_CODING_PICTOGRAM = 9;
	const DATA_CODING_ISO2022_JP = 10; // Music codes
	const DATA_CODING_KANJI = 13; // Extended Kanji JIS
	const DATA_CODING_KSC5601 = 14;
		
	// SMPPMPT v3.4 - 5.2.25 page 129
	const DEST_FLAG_SME = 1;
	const DEST_FLAG_DISTLIST = 2;
	
	// SMPPMPT v3.4 - 5.2.28 page 130
	const STATE_ENROUTE = 1;
	const STATE_DELIVERED = 2;
	const STATE_EXPIRED = 3;
	const STATE_DELETED = 4;
	const STATE_UNDELIVERABLE = 5;
	const STATE_ACCEPTED = 6;
	const STATE_UNKNOWN = 7;
	const STATE_REJECTED = 8;
	
		
	public static function getStatusMessage($statuscode)
	{
		switch ($statuscode) {
			case SMPPMPT::ESME_ROK: return 'No Error';
			case SMPPMPT::ESME_RINVMSGLEN: return 'Message Length is invalid';
			case SMPPMPT::ESME_RINVCMDLEN: return 'Command Length is invalid';
			case SMPPMPT::ESME_RINVCMDID: return 'Invalid Command ID';
			case SMPPMPT::ESME_RINVBNDSTS: return 'Incorrect BIND Status for given command';
			case SMPPMPT::ESME_RALYBND: return 'ESME Already in Bound State';
			case SMPPMPT::ESME_RINVPRTFLG: return 'Invalid Priority Flag';
			case SMPPMPT::ESME_RINVREGDLVFLG: return 'Invalid Registered Delivery Flag';
			case SMPPMPT::ESME_RSYSERR: return 'System Error';
			case SMPPMPT::ESME_RINVSRCADR: return 'Invalid Source Address';
			case SMPPMPT::ESME_RINVDSTADR: return 'Invalid Dest Addr';
			case SMPPMPT::ESME_RINVMSGID: return 'Message ID is invalid';
			case SMPPMPT::ESME_RBINDFAIL: return 'Bind Failed';
			case SMPPMPT::ESME_RINVPASWD: return 'Invalid Password';
			case SMPPMPT::ESME_RINVSYSID: return 'Invalid System ID';
			case SMPPMPT::ESME_RCANCELFAIL: return 'Cancel SM Failed';
			case SMPPMPT::ESME_RREPLACEFAIL: return 'Replace SM Failed';
			case SMPPMPT::ESME_RMSGQFUL: return 'Message Queue Full';
			case SMPPMPT::ESME_RINVSERTYP: return 'Invalid Service Type';
			case SMPPMPT::ESME_RINVNUMDESTS: return 'Invalid number of destinations';
			case SMPPMPT::ESME_RINVDLNAME: return 'Invalid Distribution List name';
			case SMPPMPT::ESME_RINVDESTFLAG: return 'Destination flag (submit_multi)';
			case SMPPMPT::ESME_RINVSUBREP: return 'Invalid ‘submit with replace’ request (i.e. submit_sm with replace_if_present_flag set)';
			case SMPPMPT::ESME_RINVESMSUBMIT: return 'Invalid esm_SUBMIT field data';
			case SMPPMPT::ESME_RCNTSUBDL: return 'Cannot Submit to Distribution List';
			case SMPPMPT::ESME_RSUBMITFAIL: return 'submit_sm or submit_multi failed';
			case SMPPMPT::ESME_RINVSRCTON: return 'Invalid Source address TON';
			case SMPPMPT::ESME_RINVSRCNPI: return 'Invalid Source address NPI';
			case SMPPMPT::ESME_RINVDSTTON: return 'Invalid Destination address TON';
			case SMPPMPT::ESME_RINVDSTNPI: return 'Invalid Destination address NPI';
			case SMPPMPT::ESME_RINVSYSTYP: return 'Invalid system_type field';
			case SMPPMPT::ESME_RINVREPFLAG: return 'Invalid replace_if_present flag';
			case SMPPMPT::ESME_RINVNUMMSGS: return 'Invalid number of messages';
			case SMPPMPT::ESME_RTHROTTLED: return 'Throttling error (ESME has exceeded allowed message limits)';
			case SMPPMPT::ESME_RINVSCHED: return 'Invalid Scheduled Delivery Time';
			case SMPPMPT::ESME_RINVEXPIRY: return 'Invalid message (Expiry time)';
			case SMPPMPT::ESME_RINVDFTMSGID: return 'Predefined Message Invalid or Not Found';
			case SMPPMPT::ESME_RX_T_APPN: return 'ESME Receiver Temporary App Error Code';
			case SMPPMPT::ESME_RX_P_APPN: return 'ESME Receiver Permanent App Error Code';
			case SMPPMPT::ESME_RX_R_APPN: return 'ESME Receiver Reject Message Error Code';
			case SMPPMPT::ESME_RQUERYFAIL: return 'query_sm request failed';
			case SMPPMPT::ESME_RINVOPTPARSTREAM: return 'Error in the optional part of the PDU Body.';
			case SMPPMPT::ESME_ROPTPARNOTALLWD: return 'Optional Parameter not allowed';
			case SMPPMPT::ESME_RINVPARLEN: return 'Invalid Parameter Length.';
			case SMPPMPT::ESME_RMISSINGOPTPARAM: return 'Expected Optional Parameter missing';
			case SMPPMPT::ESME_RINVOPTPARAMVAL: return 'Invalid Optional Parameter Value';
			case SMPPMPT::ESME_RDELIVERYFAILURE: return 'Delivery Failure (data_sm_resp)';
			case SMPPMPT::ESME_RUNKNOWNERR: return 'Unknown Error';
			default:
				return 'Unknown statuscode: '.dechex($statuscode);
		}
	}
}
	
/**
 * Primitive class for encapsulating PDUs
 * @author hd@onlinecity.dk
 */
class SmppPduMPT
{
	public $id;
	public $status;
	public $sequence;
	public $body;
	
	/**
	 * Create new generic PDU object
	 * 
	 * @param integer $id
	 * @param integer $status
	 * @param integer $sequence
	 * @param string $body
	 */
	public function __construct($id, $status, $sequence, $body)
	{
		$this->id = $id;
		$this->status = $status;
		$this->sequence = $sequence;
		$this->body = $body;
	}
}

/**
 * An extension of a SMS, with data embedded into the message part of the SMS.
 * @author hd@onlinecity.dk
 */
class SmppDeliveryReceiptMPT extends SmppSmsMPT
{
	public $id;
	public $sub;
	public $dlvrd;
	public $submitDate;
	public $doneDate;
	public $stat;
	public $err;
	public $text;
	
	/**
	 * Parse a delivery receipt formatted as specified in SMPPMPT v3.4 - Appendix B
	 * It accepts all chars except space as the message id
	 * 
	 * @throws InvalidArgumentException
	 */
	public function parseDeliveryReceipt()
	{
		$numMatches = preg_match('/^id:([^ ]+) sub:(\d{1,3}) dlvrd:(\d{3}) submit date:(\d{10,12}) done date:(\d{10,12}) stat:([A-Z ]{7}) err:(\d{2,3}) text:(.*)$/si', $this->message, $matches);
		if ($numMatches == 0) {
			throw new InvalidArgumentException('Could not parse delivery receipt: '.$this->message."\n".bin2hex($this->body));	
		}
		list($matched, $this->id, $this->sub, $this->dlvrd, $this->submitDate, $this->doneDate, $this->stat, $this->err, $this->text) = $matches;
		
		// Convert dates
		$dp = str_split($this->submitDate,2);
		$this->submitDate = gmmktime($dp[3],$dp[4],isset($dp[5]) ? $dp[5] : 0,$dp[1],$dp[2],$dp[0]);
		$dp = str_split($this->doneDate,2);
		$this->doneDate = gmmktime($dp[3],$dp[4],isset($dp[5]) ? $dp[5] : 0,$dp[1],$dp[2],$dp[0]);
	}
}

/**
 * Primitive type to represent SMSes
 * @author hd@onlinecity.dk
 */
class SmppSmsMPT extends SmppPduMPT
{
	public $service_type;
	public $source;
	public $destination;
	public $esmClass;
	public $protocolId;
	public $priorityFlag;
	public $registeredDelivery;
	public $dataCoding;
	public $message;
	public $tags;
	
	// Unused in deliver_sm
	public $scheduleDeliveryTime;
	public $validityPeriod;
	public $smDefaultMsgId;
	public $replaceIfPresentFlag;

	/**
	 * Construct a new SMS
	 * 
	 * @param integer $id
	 * @param integer $status
	 * @param integer $sequence
	 * @param string $body
	 * @param string $service_type
	 * @param Address $source
	 * @param Address $destination
	 * @param integer $esmClass
	 * @param integer $protocolId
	 * @param integer $priorityFlag
	 * @param integer $registeredDelivery
	 * @param integer $dataCoding
	 * @param string $message
	 * @param array $tags (optional)
	 * @param string $scheduleDeliveryTime (optional)
	 * @param string $validityPeriod (optional)
	 * @param integer $smDefaultMsgId (optional)
	 * @param integer $replaceIfPresentFlag (optional)
	 */
	public function __construct($id, $status, $sequence, $body, $service_type, SmppAddressMPT $source, SmppAddressMPT $destination, 
		$esmClass, $protocolId, $priorityFlag, $registeredDelivery, $dataCoding, $message, $tags, 
		$scheduleDeliveryTime=null, $validityPeriod=null, $smDefaultMsgId=null, $replaceIfPresentFlag=null)
	{
		parent::__construct($id, $status, $sequence, $body);
		$this->service_type = $service_type;
		$this->source = $source;
		$this->destination = $destination;
		$this->esmClass = $esmClass;
		$this->protocolId = $protocolId;
		$this->priorityFlag = $priorityFlag;
		$this->registeredDelivery = $registeredDelivery;
		$this->dataCoding = $dataCoding;
		$this->message = $message;
		$this->tags = $tags;
		$this->scheduleDeliveryTime = $scheduleDeliveryTime;
		$this->validityPeriod = $validityPeriod;
		$this->smDefaultMsgId = $smDefaultMsgId;
		$this->replaceIfPresentFlag = $replaceIfPresentFlag;
	}
		
}

/**
 * Primitive class for encapsulating smpp addresses
 * @author hd@onlinecity.dk
 */
class SmppAddressMPT
{
	public $ton; // type-of-number
	public $npi; // numbering-plan-indicator
	public $value;

	/**
	 * Construct a new object of class Address
	 * 
	 * @param string $value
	 * @param integer $ton
	 * @param integer $npi
	 * @throws InvalidArgumentException
	 */
	public function __construct($value,$ton=SMPPMPT::TON_UNKNOWN,$npi=SMPPMPT::NPI_UNKNOWN)
	{
		// Address-Value field may contain 10 octets (12-length-type), see 3GPP TS 23.040 v 9.3.0 - section 9.1.2.5 page 46.
		if ($ton == SMPPMPT::TON_ALPHANUMERIC && strlen($value) > 11) throw new InvalidArgumentException('Alphanumeric address may only contain 11 chars');
		if ($ton == SMPPMPT::TON_INTERNATIONAL && $npi == SMPPMPT::NPI_E164 && strlen($value) > 15) throw new InvalidArgumentException('E164 address may only contain 15 digits');
		
		$this->value = (string) $value;
		$this->ton = $ton;
		$this->npi = $npi;
	}
}

/**
 * Primitive class to represent SMPPMPT optional params, also know as TLV (Tag-Length-Value) params
 * @author hd@onlinecity.dk
 */
class SmppTagMPT
{
	public $id;
	public $length;
	public $value;
	public $type;
	
	const DEST_ADDR_SUBUNIT = 0x0005;
	const DEST_NETWORK_TYPE = 0x0006;
	const DEST_BEARER_TYPE = 0x0007;
	const DEST_TELEMATICS_ID = 0x0008;
	const SOURCE_ADDR_SUBUNIT = 0x000D;
	const SOURCE_NETWORK_TYPE = 0x000E;
	const SOURCE_BEARER_TYPE = 0x000F;
	const SOURCE_TELEMATICS_ID = 0x0010;
	const QOS_TIME_TO_LIVE = 0x0017;
	const PAYLOAD_TYPE = 0x0019;
	const ADDITIONAL_STATUS_INFO_TEXT = 0x001D;
	const RECEIPTED_MESSAGE_ID = 0x001E;
	const MS_MSG_WAIT_FACILITIES = 0x0030;
	const PRIVACY_INDICATOR = 0x0201;
	const SOURCE_SUBADDRESS = 0x0202;
	const DEST_SUBADDRESS = 0x0203;
	const USER_MESSAGE_REFERENCE = 0x0204;
	const USER_RESPONSE_CODE = 0x0205;
	const SOURCE_PORT = 0x020A;
	const DESTINATION_PORT = 0x020B;
	const SAR_MSG_REF_NUM = 0x020C;
	const LANGUAGE_INDICATOR = 0x020D;
	const SAR_TOTAL_SEGMENTS = 0x020E;
	const SAR_SEGMENT_SEQNUM = 0x020F;
	const SC_INTERFACE_VERSION = 0x0210;
	const CALLBACK_NUM_PRES_IND = 0x0302;
	const CALLBACK_NUM_ATAG = 0x0303;
	const NUMBER_OF_MESSAGES = 0x0304;
	const CALLBACK_NUM = 0x0381;
	const DPF_RESULT = 0x0420;
	const SET_DPF = 0x0421;
	const MS_AVAILABILITY_STATUS = 0x0422;
	const NETWORK_ERROR_CODE = 0x0423;
	const MESSAGE_PAYLOAD = 0x0424;
	const DELIVERY_FAILURE_REASON = 0x0425;
	const MORE_MESSAGES_TO_SEND = 0x0426;
	const MESSAGE_STATE = 0x0427;
	const USSD_SERVICE_OP = 0x0501;
	const DISPLAY_TIME = 0x1201;
	const SMS_SIGNAL = 0x1203;
	const MS_VALIDITY = 0x1204;
	const ALERT_ON_MESSAGE_DELIVERY = 0x130C;
	const ITS_REPLY_TYPE = 0x1380;
	const ITS_SESSION_INFO = 0x1383;
	
	
	/**
	 * Construct a new TLV param.
	 * The value must either be pre-packed with pack(), or a valid pack-type must be specified.
	 * 
	 * @param integer $id
	 * @param string $value
	 * @param integer $length (optional)
	 * @param string $type (optional)
	 */
	public function __construct($id, $value, $length=null, $type='a*')
	{
		$this->id = $id;
		$this->value = $value;
		$this->length = $length;
		$this->type = $type;
	}
	
	/**
	 * Get the TLV packed into a binary string for transport
	 * @return string
	 */
	public function getBinary()
	{
		return pack('nn'.$this->type, $this->id, ($this->length ? $this->length : strlen($this->value)), $this->value);
	}
}
