;;; -*- Lisp -*- - well, that's stretching a point.  code=data != data=code
; #+win64 ("C:/Users/martin/Desktop/tmp/sb-andor2-win/win/ATMCD32D.H")
; #+linux
("/usr/local/include/atmcdLXd.h")

((:structure andorcaps
 	     ("struct ANDORCAPS"
 	      (unsigned size "ULONG" "ulSize")
	      (unsigned Acq-Modes "ULONG" "ulAcqModes")
	      (unsigned Read-Modes "ULONG" "ulReadModes")
	      (unsigned Trigger-Modes "ULONG" "ulTriggerModes")
	      (unsigned Camera-Type "ULONG" "ulCameraType")
	      (unsigned Pixel-Mode "ULONG" "ulPixelMode")
	      (unsigned Set-Functions "ULONG" "ulSetFunctions")
	      (unsigned Get-Functions "ULONG" "ulGetFunctions")
	      (unsigned Features "ULONG" "ulFeatures")
	      (unsigned PCI-Card "ULONG" "ulPCICard")
	      (unsigned EMGain-Capability "ULONG" "ulEMGainCapability")
	      (unsigned FT-Read-Modes "ULONG" "ulFTReadModes")
	      )))


