(ql:quickload "split-sequence")

(defparameter *funs* 
"AbortAcquisition(void)
CancelWait(void)
CoolerOFF(void)
CoolerON(void)
DemosaicImage(WORD * grey, WORD * red, WORD * green, WORD * blue, ColorDemosaicInfo * info)
EnableKeepCleans(int iMode)
FreeInternalMemory(void)
GetAcquiredData(at_32 * arr, unsigned long size)
GetAcquiredData16(WORD * arr, unsigned long size)
GetAcquiredFloatData(float * arr, unsigned long size)
GetAcquisitionProgress(long * acc, long * series)
GetAcquisitionTimings(float * exposure, float * accumulate, float * kinetic)
GetAdjustedRingExposureTimes(int inumTimes, float * fptimes)
GetAllDMAData(at_32 * arr, unsigned long size)
GetAmpDesc(int index, char * name, int length)
GetAmpMaxSpeed(int index, float * speed)
GetAvailableCameras(long * totalCameras)
GetBackground(at_32 * arr, unsigned long size)
GetBaselineClamp(int * state)
GetBitDepth(int channel, int * depth)
GetCameraEventStatus(DWORD * camStatus)
GetCameraHandle(long cameraIndex, long * cameraHandle)
GetCameraInformation(int index, long * information)
GetCameraSerialNumber(int * number)
GetCapabilities(AndorCapabilities * caps)
GetControllerCardModel(char * controllerCardModel)
GetCountConvertWavelengthRange(float * minval, float * maxval)
GetCurrentCamera(long * cameraHandle)
GetCYMGShift(int * iXshift, int * iYShift)
GetDDGExternalOutputEnabled(at_u32 uiIndex, at_u32 * puiEnabled)
GetDDGExternalOutputPolarity(at_u32 uiIndex, at_u32 * puiPolarity)
GetDDGExternalOutputStepEnabled(at_u32 uiIndex, at_u32 * puiEnabled)
GetDDGExternalOutputTime(at_u32 uiIndex, at_u64 * puiDelay, at_u64 * puiWidth)
GetDDGGateTime(at_u64 * puiDelay, at_u64 * puiWidth)
GetDDGInsertionDelay(int * piState)
GetDDGIntelligate(int * piState)
GetDDGIOC(int * state)
GetDDGIOCFrequency(double * frequency)
GetDDGIOCNumber(unsigned long * numberPulses)
GetDDGIOCNumberRequested(at_u32 * pulses)
GetDDGIOCPeriod(at_u64 * period)
GetDDGIOCPulses(int * pulses)
GetDDGLiteGlobalControlByte(unsigned char * control)
GetDDGLiteControlByte(AT_DDGLiteChannelId channel, unsigned char * control)
GetDDGLiteInitialDelay(AT_DDGLiteChannelId channel, float * fDelay)
GetDDGLitePulseWidth(AT_DDGLiteChannelId channel, float * fWidth)
GetDDGLiteInterPulseDelay(AT_DDGLiteChannelId channel, float * fDelay)
GetDDGLitePulsesPerExposure(AT_DDGLiteChannelId channel, at_u32 * ui32Pulses)
GetDDGPulse(double wid, double resolution, double * Delay, double * Width)
GetDDGStepCoefficients(at_u32 mode, double * p1, double * p2)
GetDDGStepMode(at_u32 * mode)
GetDetector(int * xpixels, int * ypixels)
GetDICameraInfo(void * info)
GetEMCCDGain(int * gain)
GetEMGainRange(int * low, int * high)
GetExternalTriggerTermination(at_u32 * puiTermination)
GetFastestRecommendedVSSpeed(int * index, float * speed)
GetFIFOUsage(int * FIFOusage)
GetFilterMode(int * mode)
GetFKExposureTime(float * time)
GetFKVShiftSpeed(int index, int * speed)
GetFKVShiftSpeedF(int index, float * speed)
GetFrontEndStatus(int * piFlag)
GetGateMode(int * piGatemode)
GetHardwareVersion(unsigned int * PCB, unsigned int * Decode, unsigned int * dummy1, unsigned int * dummy2, unsigned int * CameraFirmwareVersion, unsigned int * CameraFirmwareBuild)
GetHeadModel(char * name)
GetHorizontalSpeed(int index, int * speed)
GetHSSpeed(int channel, int typ, int index, float * speed)
GetHVflag(int * bFlag)
GetID(int devNum, int * id)
GetImageFlip(int * iHFlip, int * iVFlip)
GetImageRotate(int * iRotate)
GetImages(long first, long last, at_32 * arr, unsigned long size, long * validfirst, long * validlast)
GetImages16(long first, long last, WORD * arr, unsigned long size, long * validfirst, long * validlast)
GetImagesPerDMA(unsigned long * images)
GetIRQ(int * IRQ)
GetKeepCleanTime(float * KeepCleanTime)
GetMaximumBinning(int ReadMode, int HorzVert, int * MaxBinning)
GetMaximumExposure(float * MaxExp)
GetMCPGain(int * piGain)
GetMCPGainRange(int * iLow, int * iHigh)
GetMCPGainTable(int iNum, int * piGain, float * pfPhotoepc)
GetMCPVoltage(int * iVoltage)
GetMinimumImageLength(int * MinImageLength)
GetMinimumNumberInSeries(int * number)
GetMostRecentColorImage16(unsigned long size, int algorithm, WORD * red, WORD * green, WORD * blue)
GetMostRecentImage(at_32 * arr, unsigned long size)
GetMostRecentImage16(WORD * arr, unsigned long size)
GetMSTimingsData(SYSTEMTIME * TimeOfStart, float * pfDifferences, int inoOfImages)
GetMetaDataInfo(SYSTEMTIME * TimeOfStart, float * pfTimeFromStart, unsigned int index)
GetMSTimingsEnabled(void)
GetNewData(at_32 * arr, unsigned long size)
GetNewData16(WORD * arr, unsigned long size)
GetNewData8(unsigned char * arr, unsigned long size)
GetNewFloatData(float * arr, unsigned long size)
GetNumberADChannels(int * channels)
GetNumberAmp(int * amp)
GetNumberAvailableImages(at_32 * first, at_32 * last)
GetNumberDDGExternalOutputs(at_u32 * puiCount)
GetNumberDevices(int * numDevs)
GetNumberFKVShiftSpeeds(int * number)
GetNumberHorizontalSpeeds(int * number)
GetNumberHSSpeeds(int channel, int typ, int * speeds)
GetNumberNewImages(long * first, long * last)
GetNumberPreAmpGains(int * noGains)
GetNumberRingExposureTimes(int * ipnumTimes)
GetNumberIO(int * iNumber)
GetNumberVerticalSpeeds(int * number)
GetNumberVSAmplitudes(int * number)
GetNumberVSSpeeds(int * speeds)
GetOldestImage(at_32 * arr, unsigned long size)
GetOldestImage16(WORD * arr, unsigned long size)
GetPhosphorStatus(int * piFlag)
GetPhysicalDMAAddress(unsigned long * Address1, unsigned long * Address2)
GetPixelSize(float * xSize, float * ySize)
GetPreAmpGain(int index, float * gain)
GetDualExposureTimes(float * exposure1, float * exposure2)
GetQE(char * sensor, float wavelength, unsigned int mode, float * QE)
GetReadOutTime(float * ReadOutTime)
GetRegisterDump(int * mode)
GetRingExposureRange(float * fpMin, float * fpMax)
GetSensitivity(int channel, int horzShift, int amplifier, int pa, float * sensitivity)
GetSizeOfCircularBuffer(long * index)
GetSlotBusDeviceFunction(DWORD * dwslot, DWORD * dwBus, DWORD * dwDevice, DWORD * dwFunction)
GetSoftwareVersion(unsigned int * eprom, unsigned int * coffile, unsigned int * vxdrev, unsigned int * vxdver, unsigned int * dllrev, unsigned int * dllver)
GetSpoolProgress(long * index)
GetStartUpTime(float * time)
GetStatus(int * status)
GetTECStatus(int * piFlag)
GetTemperature(int * temperature)
GetTemperatureF(float * temperature)
GetTemperatureRange(int * mintemp, int * maxtemp)
GetTemperatureStatus(float * SensorTemp, float * TargetTemp, float * AmbientTemp, float * CoolerVolts)
GetTotalNumberImagesAcquired(long * index)
GetIODirection(int index, int * iDirection)
GetIOLevel(int index, int * iLevel)
GetVersionInfo(AT_VersionInfoId arr, char * szVersionInfo, at_u32 ui32BufferLen)
GetVerticalSpeed(int index, int * speed)
GetVirtualDMAAddress(void ** Address1, void ** Address2)
GetVSSpeed(int index, float * speed)
GPIBReceive(int id, short address, char * text, int size)
GPIBSend(int id, short address, char * text)
I2CBurstRead(BYTE i2cAddress, long nBytes, BYTE * data)
I2CBurstWrite(BYTE i2cAddress, long nBytes, BYTE * data)
I2CRead(BYTE deviceID, BYTE intAddress, BYTE * pdata)
I2CReset(void)
I2CWrite(BYTE deviceID, BYTE intAddress, BYTE data)
IdAndorDll(void)
InAuxPort(int port, int * state)
Initialize(char * dir)
InitializeDevice(char * dir)
IsAmplifierAvailable(int iamp)
IsCoolerOn(int * iCoolerStatus)
IsCountConvertModeAvailable(int mode)
IsInternalMechanicalShutter(int * InternalShutter)
IsPreAmpGainAvailable(int channel, int amplifier, int index, int pa, int * status)
IsTriggerModeAvailable(int iTriggerMode)
Merge(const at_32 * arr, long nOrder, long nPoint, long nPixel, float * coeff, long fit, long hbin, at_32 * output, float * start, float * step_Renamed)
OutAuxPort(int port, int state)
PrepareAcquisition(void)
SaveAsBmp(char * path, char * palette, long ymin, long ymax)
SaveAsCommentedSif(char * path, char * comment)
SaveAsEDF(char * szPath, int iMode)
SaveAsFITS(char * szFileTitle, int typ)
SaveAsRaw(char * szFileTitle, int typ)
SaveAsSif(char * path)
SaveAsSPC(char * path)
SaveAsTiff(char * path, char * palette, int position, int typ)
SaveAsTiffEx(char * path, char * palette, int position, int typ, int mode)
SaveEEPROMToFile(char * cFileName)
SaveToClipBoard(char * palette)
SelectDevice(int devNum)
SendSoftwareTrigger(void)
SetAccumulationCycleTime(float time)
SetAcqStatusEvent(HANDLE statusEvent)
SetAcquisitionMode(int mode)
SetAcquisitionType(int typ)
SetADChannel(int channel)
SetAdvancedTriggerModeState(int iState)
SetBackground(at_32 * arr, unsigned long size)
SetBaselineClamp(int state)
SetBaselineOffset(int offset)
SetCameraStatusEnable(DWORD Enable)
SetComplexImage(int numAreas, int * areas)
SetCoolerMode(int mode)
SetCountConvertMode(int Mode)
SetCountConvertWavelength(float wavelength)
SetCropMode(int active, int cropHeight, int reserved)
SetCurrentCamera(long cameraHandle)
SetCustomTrackHBin(int bin)
SetDataType(int typ)
SetDACOutput(int iOption, int iResolution, int iValue)
SetDACOutputScale(int iScale)
SetDDGAddress(BYTE t0, BYTE t1, BYTE t2, BYTE t3, BYTE address)
SetDDGExternalOutputEnabled(at_u32 uiIndex, at_u32 uiEnabled)
SetDDGExternalOutputPolarity(at_u32 uiIndex, at_u32 uiPolarity)
SetDDGExternalOutputStepEnabled(at_u32 uiIndex, at_u32 uiEnabled)
SetDDGExternalOutputTime(at_u32 uiIndex, at_u64 uiDelay, at_u64 uiWidth)
SetDDGGain(int gain)
SetDDGGateStep(double step_Renamed)
SetDDGGateTime(at_u64 uiDelay, at_u64 uiWidth)
SetDDGInsertionDelay(int state)
SetDDGIntelligate(int state)
SetDDGIOC(int state)
SetDDGIOCFrequency(double frequency)
SetDDGIOCNumber(unsigned long numberPulses)
SetDDGIOCPeriod(at_u64 period)
SetDDGLiteGlobalControlByte(unsigned char control)
SetDDGLiteControlByte(AT_DDGLiteChannelId channel, unsigned char control)
SetDDGLiteInitialDelay(AT_DDGLiteChannelId channel, float fDelay)
SetDDGLitePulseWidth(AT_DDGLiteChannelId channel, float fWidth)
SetDDGLiteInterPulseDelay(AT_DDGLiteChannelId channel, float fDelay)
SetDDGLitePulsesPerExposure(AT_DDGLiteChannelId channel, at_u32 ui32Pulses)
SetDDGStepCoefficients(at_u32 mode, double p1, double p2)
SetDDGStepMode(at_u32 mode)
SetDDGTimes(double t0, double t1, double t2)
SetDDGTriggerMode(int mode)
SetDDGVariableGateStep(int mode, double p1, double p2)
SetDelayGenerator(int board, short address, int typ)
SetDMAParameters(int MaxImagesPerDMA, float SecondsPerDMA)
SetDriverEvent(HANDLE driverEvent)
SetEMAdvanced(int state)
SetEMCCDGain(int gain)
SetEMClockCompensation(int EMClockCompensationFlag)
SetEMGainMode(int mode)
SetExposureTime(float time)
SetExternalTriggerTermination(at_u32 uiTermination)
SetFanMode(int mode)
SetFastExtTrigger(int mode)
SetFastKinetics(int exposedRows, int seriesLength, float time, int mode, int hbin, int vbin)
SetFastKineticsEx(int exposedRows, int seriesLength, float time, int mode, int hbin, int vbin, int offset)
SetFilterMode(int mode)
SetFilterParameters(int width, float sensitivity, int range, float accept, int smooth, int noise)
SetFKVShiftSpeed(int index)
SetFPDP(int state)
SetFrameTransferMode(int mode)
SetFrontEndEvent(HANDLE driverEvent)
SetFullImage(int hbin, int vbin)
SetFVBHBin(int bin)
SetGain(int gain)
SetGate(float delay, float width, float stepRenamed)
SetGateMode(int gatemode)
SetHighCapacity(int state)
SetHorizontalSpeed(int index)
SetHSSpeed(int typ, int index)
SetImage(int hbin, int vbin, int hstart, int hend, int vstart, int vend)
SetImageFlip(int iHFlip, int iVFlip)
SetImageRotate(int iRotate)
SetIsolatedCropMode(int active, int cropheight, int cropwidth, int vbin, int hbin)
SetKineticCycleTime(float time)
SetMCPGain(int gain)
SetMCPGating(int gating)
SetMessageWindow(HWND wnd)
SetMetaData(int state)
SetMultiTrack(int number, int height, int offset, int * bottom, int * gap)
SetMultiTrackHBin(int bin)
SetMultiTrackHRange(int iStart, int iEnd)
SetMultiTrackScan(int trackHeight, int numberTracks, int iSIHStart, int iSIHEnd, int trackHBinning, int trackVBinning, int trackGap, int trackOffset, int trackSkip, int numberSubFrames)
SetNextAddress(at_32 * data, long lowAdd, long highAdd, long length, long physical)
SetNextAddress16(at_32 * data, long lowAdd, long highAdd, long length, long physical)
SetNumberAccumulations(int number)
SetNumberKinetics(int number)
SetNumberPrescans(int iNumber)
SetOutputAmplifier(int typ)
SetOverlapMode(int mode)
SetPCIMode(int mode, int value)
SetPhotonCounting(int state)
SetPhotonCountingThreshold(long min, long max)
SetPhosphorEvent(HANDLE driverEvent)
SetPhotonCountingDivisions(at_u32 noOfDivisions, at_32 * divisions)
SetPixelMode(int bitdepth, int colormode)
SetPreAmpGain(int index)
SetDualExposureTimes(float expTime1, float expTime2)
SetDualExposureMode(int mode)
SetRandomTracks(int numTracks, int * areas)
SetReadMode(int mode)
SetRegisterDump(int mode)
SetRingExposureTimes(int numTimes, float * times)
SetSaturationEvent(HANDLE saturationEvent)
SetShutter(int typ, int mode, int closingtime, int openingtime)
SetShutterEx(int typ, int mode, int closingtime, int openingtime, int extmode)
SetShutters(int typ, int mode, int closingtime, int openingtime, int exttype, int extmode, int dummy1, int dummy2)
SetSifComment(char * comment)
SetSingleTrack(int centre, int height)
SetSingleTrackHBin(int bin)
SetSpool(int active, int method, char * path, int framebuffersize)
SetStorageMode(long mode)
SetTECEvent(HANDLE driverEvent)
SetTemperature(int temperature)
SetTemperatureEvent(HANDLE temperatureEvent)
SetTriggerMode(int mode)
SetTriggerInvert(int mode)
GetTriggerLevelRange(float * minimum, float * maximum)
SetTriggerLevel(float f_level)
SetIODirection(int index, int iDirection)
SetIOLevel(int index, int iLevel)
SetUserEvent(HANDLE userEvent)
SetUSGenomics(long width, long height)
SetVerticalRowBuffer(int rows)
SetVerticalSpeed(int index)
SetVirtualChip(int state)
SetVSAmplitude(int index)
SetVSSpeed(int index)
ShutDown(void)
StartAcquisition(void)
UnMapPhysicalAddress(void)
WaitForAcquisition(void)
WaitForAcquisitionByHandle(long cameraHandle)
WaitForAcquisitionByHandleTimeOut(long cameraHandle, int iTimeOutMs)
WaitForAcquisitionTimeOut(int iTimeOutMs)
WhiteBalance(WORD * wRed, WORD * wGreen, WORD * wBlue, float * fRelR, float * fRelB, WhiteBalanceInfo * info)
OA_Initialize(const char * const pcFilename, unsigned int uiFileNameLen)
OA_EnableMode(const char * const pcModeName)
OA_GetModeAcqParams(const char * const pcModeName, char * const pcListOfParams)
OA_GetUserModeNames(char * pcListOfModes)
OA_GetPreSetModeNames(char * pcListOfModes)
OA_GetNumberOfUserModes(unsigned int * const puiNumberOfModes)
OA_GetNumberOfPreSetModes(unsigned int * const puiNumberOfModes)
OA_GetNumberOfAcqParams(const char * const pcModeName, unsigned int * const puiNumberOfParams)
OA_AddMode(char * pcModeName, unsigned int uiModeNameLen, char * pcModeDescription, unsigned int uiModeDescriptionLen)
OA_WriteToFile(const char * const pcFileName, unsigned int uiFileNameLen)
OA_DeleteMode(const char * const pcModeName, unsigned int uiModeNameLen)
OA_SetInt(const char * const pcModeName, const char * pcModeParam, const int iIntValue)
OA_SetFloat(const char * const pcModeName, const char * pcModeParam, const float fFloatValue)
OA_SetString(const char * const pcModeName, const char * pcModeParam, char * pcStringValue, const unsigned int uiStringLen)
OA_GetInt(const char * const pcModeName, const char * const pcModeParam, int * iIntValue)
OA_GetFloat(const char * const pcModeName, const char * const pcModeParam, float * fFloatValue)
OA_GetString(const char * const pcModeName, const char * const pcModeParam, char * pcStringValue, const unsigned int uiStringLen)
Filter_SetMode(unsigned int mode)
Filter_GetMode(unsigned int * mode)
Filter_SetThreshold(float threshold)
Filter_GetThreshold(float * threshold)
Filter_SetDataAveragingMode(int mode)
Filter_GetDataAveragingMode(int * mode)
Filter_SetAveragingFrameCount(int frames)
Filter_GetAveragingFrameCount(int * frames)
Filter_SetAveragingFactor(int averagingFactor)
Filter_GetAveragingFactor(int * averagingFactor)
PostProcessNoiseFilter(at_32 * pInputImage, at_32 * pOutputImage, int iOutputBufferSize, int iBaseline, int iMode, float fThreshold, int iHeight, int iWidth)
PostProcessCountConvert(at_32 * pInputImage, at_32 * pOutputImage, int iOutputBufferSize, int iNumImages, int iBaseline, int iMode, int iEmGain, float fQE, float fSensitivity, int iHeight, int iWidth)
PostProcessPhotonCounting(at_32 * pInputImage, at_32 * pOutputImage, int iOutputBufferSize, int iNumImages, int iNumframes, int iNumberOfThresholds, float * pfThreshold, int iHeight, int iWidth)
PostProcessDataAveraging(at_32 * pInputImage, at_32 * pOutputImage, int iOutputBufferSize, int iNumImages, int iAveragingFilterMode, int iHeight, int iWidth, int iFrameCount, int iAveragingFactor)
")

;; split at newlines
(defparameter *funs-split*
 (split-sequence:split-sequence #\Newline *funs*))

;; split at first (
(defparameter *funs-parm*
 (loop for l in *funs-split* collect
      (split-sequence:split-sequence #\( l)))

;; split parameterlist at ,
(defparameter *funs-parm2*
  (loop for (f parm) in *funs-parm* collect
       (list f (split-sequence:split-sequence #\, parm))))

;; remove ) at the end of parameter list
(defparameter *funs-parm3*
  (loop for (f parm) in *funs-parm2* collect
       (flet ((remove-trailing-parenthesis (ls)
		(if (= 1 (length ls))
		    nil ;; replace "void" with nil (list (first (split-sequence:SPLIT-SEQUENCE #\) (first ls))))
		    (append (butlast ls) (list (car (split-sequence:SPLIT-SEQUENCE #\) (car (last ls)))))))))
	 (list f (remove-trailing-parenthesis parm)))))

;; remove leading space at each parameter
(defparameter *funs-parm4*
  (loop for (f parms) in *funs-parm3* collect
       (list f (mapcar #'(lambda (s) (string-left-trim '(#\Space) s))  parms))))

;; split each of the parameters at space and move the last element (name) to the front
(defparameter *funs-parm5*
  (loop for (f parms) in *funs-parm4* collect
       (list f (mapcar #'(lambda (p) (let ((spli (split-sequence:SPLIT-SEQUENCE #\Space p)))
				       (list (car (last spli)) (butlast spli))))
		       parms))))

;; if the last element of the type list is "*" write sb-alien:* in front
(defparameter *funs-parm6*
  (loop for (f parms) in *funs-parm5* collect
       
       (list f
	     (mapcar #'(lambda (p) (destructuring-bind (name type) p
				     (list name 
					   (if (string= "*" (car (last type)))
					       (list 'sb-alien:* (butlast type))
					       type))))
		     parms))))



;; i want to convert the functions names to look like this:
;; GetAcquiredData16 -> get-acquired-data16
;; CoolerOFF -> cooler-off
;; GetAllDMAData -> get-all-dma-data
;; GetCYMGShift -> get-cymg-shift
;; GetDDGIOC -> get-ddgioc

;; if not at first character and character changes case from lower to
;; upper, insert hyphen

;; if not at first or second character and character changes from
;; upper to lower and there are at least 2 upper, insert hyphen before
;; current character


(defun lispify-camelcase (m)
  (let ((res nil)
	(n (length m)))
    (dotimes (i n)
      (cond 
	((and (< 0 i)
	      (lower-case-p (char m (1- i)))
	      (upper-case-p (char m i)))
	 (push #\- res)
	 (push (char m i) res))
	((and (< 1 i)
	      (upper-case-p (char m (- i 2)))
	      (upper-case-p (char m (- i 1)))
	      (lower-case-p (char m i)))
	 (pop res) ;; remove last big letter from stack
	 (push #\- res) ;; and put a hyphen in front
	 (push (char m (- i 1)) res)
	 (push (char m i) res))
	(t (push (char m i) res))))
    (string-upcase
     (make-array (length res) :element-type 'character :initial-contents (reverse res)))))

#+nil
(loop for i in '("GetAcquiredData16"
		 "CoolerOFF"
		 "GetAllDMAData"
		 "GetCYMGShift"
		 "GetDDGIOC")
     collect
     (lispify-camelcase i))

(defparameter *s*
 '("GetAcquiredData16" (("arr" (* ("WORD"))) ("size" ("unsigned" "long")))))

;; this is how it is supposed to look like:
#+nil
(sb-alien:define-alien-routine ("GetAcquiredData16" get-acquired-data16) 
    unsigned-int
  (arr (sb-alien:* word))
  (size sb-alien:unsigned-long))

(defun string->type (ls)
  (cond 
    ((equal (car ls) '*) (list '* (string->type (car (cdr ls)))))
    ((equal '("char") ls) 'char)
    ((or (equal '("unsigned" "char") ls)
	 (equal '("BYTE") ls)) 'unsigned-char)

    ((or (equal '("WORD") ls) ;; from windef.h 
	 (equal '("unsigned" "short") ls)) 'unsigned-short)

    ((equal '("short") ls) 'short)
    ((equal '("unsigned" "int") ls) 'unsigned-int)
    ((equal '("int") ls) 'int)

    ((or (equal '("at_u32") ls)
	 (equal '("unsigned" "long") ls)
	 (equal '("DWORD") ls)) 'unsigned-long)
    ((or (equal '("at_32") ls)
	 (equal '("long") ls)) 'long)

    ((or (equal '("at_u64") ls)
	 (equal '("unsigned" "long" "long") ls)) 'unsigned-long-long)
    ((or (equal '("at_64") ls)
	 (equal '("long" "long") ls)) 'long-long)

    ((equal '("float") ls) 'float)
    ((equal '("double") ls) 'double)
    
    (t (format t "unknown type: ~a~%" ls))))

(defun generate-function-definition (spec)
  (destructuring-bind (c-name params) spec
    `(sb-alien:define-alien-routine (,c-name ,(intern (lispify-camelcase (format nil "~a*" c-name))))
	 'unsigned-int
	 ,@(loop for p in params collect
		(destructuring-bind (name type) p
		  (when (string->type type)
		   (list (intern (string-upcase name))
			 (string->type type))))))))

#+nil
(generate-function-definition *s*)

(defparameter *fun-defs* 
  (mapcar #'generate-function-definition (butlast *funs-parm6*)))


(defparameter *consts*
  '(AT_DDGLite_ControlBit_GlobalEnable   #x01
    AT_DDGLite_ControlBit_ChannelEnable  #x01
    AT_DDGLite_ControlBit_FreeRun        #x02
    AT_DDGLite_ControlBit_DisableOnFrame #x04
    AT_DDGLite_ControlBit_RestartOnFire  #x08
    AT_DDGLite_ControlBit_Invert         #x10
    AT_DDGLite_ControlBit_EnableOnFire   #x20
    AT_DDG_POLARITY_POSITIVE  0
    AT_DDG_POLARITY_NEGATIVE  1
    AT_DDG_TERMINATION_50OHMS 0
    AT_DDG_TERMINATION_HIGHZ  1
    AT_STEPMODE_CONSTANT      0
    AT_STEPMODE_EXPONENTIAL   1
    AT_STEPMODE_LOGARITHMIC   2
    AT_STEPMODE_LINEAR        3
    AT_STEPMODE_OFF           100
    AT_GATEMODE_FIRE_AND_GATE 0
    AT_GATEMODE_FIRE_ONLY     1
    AT_GATEMODE_GATE_ONLY     2
    AT_GATEMODE_CW_ON         3
    AT_GATEMODE_CW_OFF        4
    AT_GATEMODE_DDG           5
    DRV_ERROR_CODES 20001
    DRV_SUCCESS 20002
    DRV_VXDNOTINSTALLED 20003
    DRV_ERROR_SCAN 20004
    DRV_ERROR_CHECK_SUM 20005
    DRV_ERROR_FILELOAD 20006
    DRV_UNKNOWN_FUNCTION 20007
    DRV_ERROR_VXD_INIT 20008
    DRV_ERROR_ADDRESS 20009
    DRV_ERROR_PAGELOCK 20010
    DRV_ERROR_PAGEUNLOCK 20011
    DRV_ERROR_BOARDTEST 20012
    DRV_ERROR_ACK 20013
    DRV_ERROR_UP_FIFO 20014
    DRV_ERROR_PATTERN 20015
    DRV_ACQUISITION_ERRORS 20017
    DRV_ACQ_BUFFER 20018
    DRV_ACQ_DOWNFIFO_FULL 20019
    DRV_PROC_UNKONWN_INSTRUCTION 20020
    DRV_ILLEGAL_OP_CODE 20021
    DRV_KINETIC_TIME_NOT_MET 20022
    DRV_ACCUM_TIME_NOT_MET 20023
    DRV_NO_NEW_DATA 20024
    DRV_PCI_DMA_FAIL 20025
    DRV_SPOOLERROR 20026
    DRV_SPOOLSETUPERROR 20027
    DRV_FILESIZELIMITERROR 20028
    DRV_ERROR_FILESAVE 20029
    DRV_TEMPERATURE_CODES 20033
    DRV_TEMPERATURE_OFF 20034
    DRV_TEMPERATURE_NOT_STABILIZED 20035
    DRV_TEMPERATURE_STABILIZED 20036
    DRV_TEMPERATURE_NOT_REACHED 20037
    DRV_TEMPERATURE_OUT_RANGE 20038
    DRV_TEMPERATURE_NOT_SUPPORTED 20039
    DRV_TEMPERATURE_DRIFT 20040
    DRV_TEMP_CODES 20033
    DRV_TEMP_OFF 20034
    DRV_TEMP_NOT_STABILIZED 20035
    DRV_TEMP_STABILIZED 20036
    DRV_TEMP_NOT_REACHED 20037
    DRV_TEMP_OUT_RANGE 20038
    DRV_TEMP_NOT_SUPPORTED 20039
    DRV_TEMP_DRIFT 20040
    DRV_GENERAL_ERRORS 20049
    DRV_INVALID_AUX 20050
    DRV_COF_NOTLOADED 20051
    DRV_FPGAPROG 20052
    DRV_FLEXERROR 20053
    DRV_GPIBERROR 20054
    DRV_EEPROMVERSIONERROR 20055
    DRV_DATATYPE 20064
    DRV_DRIVER_ERRORS 20065
    DRV_P1INVALID 20066
    DRV_P2INVALID 20067
    DRV_P3INVALID 20068
    DRV_P4INVALID 20069
    DRV_INIERROR 20070
    DRV_COFERROR 20071
    DRV_ACQUIRING 20072
    DRV_IDLE 20073
    DRV_TEMPCYCLE 20074
    DRV_NOT_INITIALIZED 20075
    DRV_P5INVALID 20076
    DRV_P6INVALID 20077
    DRV_INVALID_MODE 20078
    DRV_INVALID_FILTER 20079
    DRV_I2CERRORS 20080
    DRV_I2CDEVNOTFOUND 20081
    DRV_I2CTIMEOUT 20082
    DRV_P7INVALID 20083
    DRV_P8INVALID 20084
    DRV_P9INVALID 20085
    DRV_P10INVALID 20086
    DRV_P11INVALID 20087
    DRV_USBERROR 20089
    DRV_IOCERROR 20090
    DRV_VRMVERSIONERROR 20091
    DRV_USB_INTERRUPT_ENDPOINT_ERROR 20093
    DRV_RANDOM_TRACK_ERROR 20094
    DRV_INVALID_TRIGGER_MODE 20095
    DRV_LOAD_FIRMWARE_ERROR 20096
    DRV_DIVIDE_BY_ZERO_ERROR 20097
    DRV_INVALID_RINGEXPOSURES 20098
    DRV_BINNING_ERROR 20099
    DRV_INVALID_AMPLIFIER 20100
    DRV_INVALID_COUNTCONVERT_MODE 20101
    DRV_ERROR_NOCAMERA 20990
    DRV_NOT_SUPPORTED 20991
    DRV_NOT_AVAILABLE 20992
    DRV_ERROR_MAP 20115
    DRV_ERROR_UNMAP 20116
    DRV_ERROR_MDL 20117
    DRV_ERROR_UNMDL 20118
    DRV_ERROR_BUFFSIZE 20119
    DRV_ERROR_NOHANDLE 20121
    DRV_GATING_NOT_AVAILABLE 20130
    DRV_FPGA_VOLTAGE_ERROR 20131
    DRV_OW_CMD_FAIL 20150
    DRV_OWMEMORY_BAD_ADDR 20151
    DRV_OWCMD_NOT_AVAILABLE 20152
    DRV_OW_NO_SLAVES 20153
    DRV_OW_NOT_INITIALIZED 20154
    DRV_OW_ERROR_SLAVE_NUM 20155
    DRV_MSTIMINGS_ERROR 20156
    DRV_OA_NULL_ERROR 20173
    DRV_OA_PARSE_DTD_ERROR 20174
    DRV_OA_DTD_VALIDATE_ERROR 20175
    DRV_OA_FILE_ACCESS_ERROR 20176
    DRV_OA_FILE_DOES_NOT_EXIST 20177
    DRV_OA_XML_INVALID_OR_NOT_FOUND_ERROR 20178
    DRV_OA_PRESET_FILE_NOT_LOADED 20179
    DRV_OA_USER_FILE_NOT_LOADED 20180
    DRV_OA_PRESET_AND_USER_FILE_NOT_LOADED 20181
    DRV_OA_INVALID_FILE 20182
    DRV_OA_FILE_HAS_BEEN_MODIFIED 20183
    DRV_OA_BUFFER_FULL 20184
    DRV_OA_INVALID_STRING_LENGTH 20185
    DRV_OA_INVALID_CHARS_IN_NAME 20186
    DRV_OA_INVALID_NAMING 20187
    DRV_OA_GET_CAMERA_ERROR 20188
    DRV_OA_MODE_ALREADY_EXISTS 20189
    DRV_OA_STRINGS_NOT_EQUAL 20190
    DRV_OA_NO_USER_DATA 20191
    DRV_OA_VALUE_NOT_SUPPORTED 20192
    DRV_OA_MODE_DOES_NOT_EXIST 20193
    DRV_OA_CAMERA_NOT_SUPPORTED 20194
    DRV_OA_FAILED_TO_GET_MODE 20195
    DRV_PROCESSING_FAILED 20211
    AC_ACQMODE_SINGLE 1
    AC_ACQMODE_VIDEO 2
    AC_ACQMODE_ACCUMULATE 4
    AC_ACQMODE_KINETIC 8
    AC_ACQMODE_FRAMETRANSFER 16
    AC_ACQMODE_FASTKINETICS 32
    AC_ACQMODE_OVERLAP 64
    AC_READMODE_FULLIMAGE 1
    AC_READMODE_SUBIMAGE 2
    AC_READMODE_SINGLETRACK 4
    AC_READMODE_FVB 8
    AC_READMODE_MULTITRACK 16
    AC_READMODE_RANDOMTRACK 32
    AC_READMODE_MULTITRACKSCAN 64
    AC_TRIGGERMODE_INTERNAL 1
    AC_TRIGGERMODE_EXTERNAL 2
    AC_TRIGGERMODE_EXTERNAL_FVB_EM 4
    AC_TRIGGERMODE_CONTINUOUS 8
    AC_TRIGGERMODE_EXTERNALSTART 16
    AC_TRIGGERMODE_EXTERNALEXPOSURE 32
    AC_TRIGGERMODE_INVERTED #x40
    AC_TRIGGERMODE_BULB 32
    AC_CAMERATYPE_PDA 0
    AC_CAMERATYPE_IXON 1
    AC_CAMERATYPE_ICCD 2
    AC_CAMERATYPE_EMCCD 3
    AC_CAMERATYPE_CCD 4
    AC_CAMERATYPE_ISTAR 5
    AC_CAMERATYPE_VIDEO 6
    AC_CAMERATYPE_IDUS 7
    AC_CAMERATYPE_NEWTON 8
    AC_CAMERATYPE_SURCAM 9
    AC_CAMERATYPE_USBICCD 10
    AC_CAMERATYPE_LUCA 11
    AC_CAMERATYPE_RESERVED 12
    AC_CAMERATYPE_IKON 13
    AC_CAMERATYPE_INGAAS 14
    AC_CAMERATYPE_IVAC 15
    AC_CAMERATYPE_UNPROGRAMMED 16
    AC_CAMERATYPE_CLARA 17
    AC_CAMERATYPE_USBISTAR 18
    AC_CAMERATYPE_SIMCAM 19
    AC_CAMERATYPE_SCMOS 20
    AC_CAMERATYPE_EXTREME 21
    AC_PIXELMODE_8BIT 1
    AC_PIXELMODE_14BIT 2
    AC_PIXELMODE_16BIT 4
    AC_PIXELMODE_32BIT 8
    AC_PIXELMODE_MONO #x000000
    AC_PIXELMODE_RGB #x010000
    AC_PIXELMODE_CMY #x020000
    AC_SETFUNCTION_VREADOUT #x01
    AC_SETFUNCTION_HREADOUT #x02
    AC_SETFUNCTION_TEMPERATURE #x04
    AC_SETFUNCTION_MCPGAIN #x08
    AC_SETFUNCTION_EMCCDGAIN #x10
    AC_SETFUNCTION_BASELINECLAMP #x20
    AC_SETFUNCTION_VSAMPLITUDE #x40
    AC_SETFUNCTION_HIGHCAPACITY #x80
    AC_SETFUNCTION_BASELINEOFFSET #x0100
    AC_SETFUNCTION_PREAMPGAIN #x0200
    AC_SETFUNCTION_CROPMODE #x0400
    AC_SETFUNCTION_DMAPARAMETERS #x0800
    AC_SETFUNCTION_HORIZONTALBIN #x1000
    AC_SETFUNCTION_MULTITRACKHRANGE #x2000
    AC_SETFUNCTION_RANDOMTRACKNOGAPS #x4000
    AC_SETFUNCTION_EMADVANCED #x8000
    AC_SETFUNCTION_GATEMODE #x010000
    AC_SETFUNCTION_DDGTIMES #x020000
    AC_SETFUNCTION_IOC #x040000
    AC_SETFUNCTION_INTELLIGATE #x080000
    AC_SETFUNCTION_INSERTION_DELAY #x100000
    AC_SETFUNCTION_GATESTEP #x200000
    AC_SETFUNCTION_TRIGGERTERMINATION #x400000
    AC_SETFUNCTION_GAIN 8
    AC_SETFUNCTION_ICCDGAIN 8
    AC_GETFUNCTION_TEMPERATURE #x01
    AC_GETFUNCTION_TARGETTEMPERATURE #x02
    AC_GETFUNCTION_TEMPERATURERANGE #x04
    AC_GETFUNCTION_DETECTORSIZE #x08
    AC_GETFUNCTION_MCPGAIN #x10
    AC_GETFUNCTION_EMCCDGAIN #x20
    AC_GETFUNCTION_HVFLAG #x40
    AC_GETFUNCTION_GATEMODE #x80
    AC_GETFUNCTION_DDGTIMES #x0100
    AC_GETFUNCTION_IOC #x0200
    AC_GETFUNCTION_INTELLIGATE #x0400
    AC_GETFUNCTION_INSERTION_DELAY #x0800
    AC_GETFUNCTION_GATESTEP #x1000
    AC_GETFUNCTION_PHOSPHORSTATUS #x2000
    AC_GETFUNCTION_MCPGAINTABLE #x4000
    AC_GETFUNCTION_BASELINECLAMP #x8000
    AC_GETFUNCTION_GAIN #x10
    AC_GETFUNCTION_ICCDGAIN #x10
    AC_FEATURES_POLLING 1
    AC_FEATURES_EVENTS 2
    AC_FEATURES_SPOOLING 4
    AC_FEATURES_SHUTTER 8
    AC_FEATURES_SHUTTEREX 16
    AC_FEATURES_EXTERNAL_I2C 32
    AC_FEATURES_SATURATIONEVENT 64
    AC_FEATURES_FANCONTROL 128
    AC_FEATURES_MIDFANCONTROL 256
    AC_FEATURES_TEMPERATUREDURINGACQUISITION 512
    AC_FEATURES_KEEPCLEANCONTROL 1024
    AC_FEATURES_DDGLITE #x0800
    AC_FEATURES_FTEXTERNALEXPOSURE #x1000
    AC_FEATURES_KINETICEXTERNALEXPOSURE #x2000
    AC_FEATURES_DACCONTROL #x4000
    AC_FEATURES_METADATA #x8000
    AC_FEATURES_IOCONTROL #x10000
    AC_FEATURES_PHOTONCOUNTING #x20000
    AC_FEATURES_COUNTCONVERT #x40000
    AC_FEATURES_DUALMODE #x80000
    AC_FEATURES_OPTACQUIRE #x100000
    AC_FEATURES_REALTIMESPURIOUSNOISEFILTER #x200000
    AC_FEATURES_POSTPROCESSSPURIOUSNOISEFILTER #x400000
    AC_EMGAIN_8BIT 1
    AC_EMGAIN_12BIT 2
    AC_EMGAIN_LINEAR12 4
    AC_EMGAIN_REAL12 8))

(loop for i below (length *consts*) by 2 collect
     `(defconstant ,(elt *consts* i) ,(elt *consts* (1+ i)))) 