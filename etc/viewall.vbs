'********************************************************************
'*
'* File:           ViewAll.vbs
'* Created:        March 1999 combined March 2002
'* Version:        1.1
'*
'*  Main Function:  Displays information on lots of stuff.
'* Function intParseCmdLine()
'* 	Sub ShowUsage()
'* General Routines: ------------------------------
'* Function strFormatMOFTime(strDate)
'* Function strLimitStringLengthSlashes(strInput, intMaxLength, intIndent)
'* 
'* Function strPackString()
'* Function blnConnect()
'* 	Sub	VerifyHostIsCscript()
'* 	Sub 	WriteLine()
'* Function blnErrorOccurred()
'* Function blnOpenFile
'*
'*  ViewAll.vbs   [/S <server>] [/U <username>] [/W <password>] 
'*                [/O <outputfile>]
'*
'* by Wilson Mar based on Copyright (C) 1998 Microsoft Corporation
'*
'********************************************************************

OPTION EXPLICIT

    'Define constants
    CONST CONST_ERROR                   = 0
    CONST CONST_WSCRIPT                 = 1
    CONST CONST_CSCRIPT                 = 2
    CONST CONST_SHOW_USAGE              = 3
    CONST CONST_PROCEED                 = 4

    'Declare variables
    Dim intOpMode, i
    Dim strServer, strUserName, strPassword, strOutputFile
    Dim blnVerbose, blnDetails 
    Dim strDomain, strName
    Dim blnQuery, blnLockout, blnDisabled
    Dim intSortProperty, intWidth, intSortOrder
    ReDim strProperties(2), intWidths(2)

    'Make sure the host is csript, if not then abort
    Call VerifyHostIsCscript()

    'Parse the command line
    intOpMode = intParseCmdLine(strServer     ,  _
                                strUserName   ,  _
                                strPassword   ,  _
                                strOutputFile    )
    Select Case intOpMode

        Case CONST_SHOW_USAGE
            Call ShowUsage()

        Case CONST_PROCEED                 
            Call ListOs(strServer     , _
                        strOutputFile , _
                        strUserName   , _
                        strPassword     )
            Call LstCompSys(strServer    , _
                           strOutputFile , _
                           strUserName   , _
                           strPassword     )
            Call GetProcInfo(strServer    , _
                             strOutputFile , _
                             strUserName   , _
                             strPassword     )
            Call GetCacheInfo(strServer     , _
                              strOutputFile , _
                              strUserName   , _
                              strPassword     )
            Call CheckBios(strServer     , _
                           strOutputFile , _
                           strUserName   , _
                           strPassword     )
            Call LstDMAs(strServer     , _
                         strOutputFile , _
                         strUserName   , _
                         strPassword     )
            Call LstIRQs(strServer     , _
                         strOutputFile , _
                         strUserName   , _
                         strPassword     )
            Call GetBusInfo(strServer,      _
                            strOutputFile,  _
                            strUserName,    _
                            strPassword     )
            Call LstKeyBoard(strServer    , _
                             strOutputFile , _
                             strUserName   , _
                             strPassword     )
            Call GetPointDevInfo(strServer     , _
                                 strOutputFile , _
                                 strUserName   , _
                                 strPassword     )
            Call LstDpConInfo(strServer     , _
                              strOutputFile , _
                              strUserName   , _
                              strPassword     )
            Call GetDispInfo(strServer     , _
                             strOutputFile , _
                             strUserName   , _
                             strPassword     )
            Call GetPPortInfo(strServer     , _
                              strOutputFile , _
                              strUserName   , _
                              strPassword     )
            Call LstSerialInfo(strServer    , _
                              strOutputFile , _
                              strUserName   , _
                              strPassword     )
            Call GetSndDevInfo(strServer     , _
                               strOutputFile , _
                               strUserName   , _
                               strPassword     )
            Call CdRomDrives(strServer    , _
                            strOutputFile , _
                            strUserName   , _
                            strPassword     )
' device.vbs list too long
' devicemem.vbs - instances of Win32_DeviceMemoryAddress on a machine!

            Call Drives(strServer     , _
                        strOutputFile , _
                        strUserName   , _
                        strPassword     )
            Call GetSCSIs(strServer     , _
                          strOutputFile , _
                          strUserName   , _
                          strPassword     )
            Call GetDskPartInf(strServer     , _
                               strOutputFile , _
                               strUserName   , _
                               strPassword     )
' pagefile.vbs
            Call GetTapeDrive(strServer     , _
                              strOutputFile , _
                              strUserName   , _
                              strPassword     )
            Call ListSpace(strServer     , _
                           strOutputFile , _
                           strUserName   , _
                           strPassword     )
            Call ListFreeSpace(strServer     , _
                               strOutputFile , _
                               strUserName   , _
                               strPassword     )
            Call BootConfig(strServer,      _
                            strOutputFile,  _
                            strUserName,    _
                            strPassword     )
            Call CodecFiles(strServer     , strOutputFile , strUserName   , strPassword   , _
                            blnDetails      )

            Call ListAdapters(strServer     , _
                              strOutputFile , _
                              strUserName   , _
                              strPassword     )
            Call GetBindings(strServer     , _
                             strOutputFile , _
                             strUserName   , _
                             strPassword     )
            Call NetConnections(strServer     , _
                                strOutputFile , _
                                strUserName   , _
                                strPassword     )
' enabledhcp.vbs
            Call LstSysAccount(strServer     , _
                               strOutputFile , _
                               strUserName   , _
                               strPassword     )
            Call UserAccounts(strServer     ,  _
                              strOutputFile ,  _
                              strUserName   ,  _
                              strPassword   ,  _
                              blnQuery      ,  _
                              strDomain     ,  _
                              strName       ,  _
                              blnLockout    ,  _
                              blnDisabled      )
            Call LstDeskTop(strServer    , _
                           strOutputFile , _
                           strUserName   , _
                           strPassword     )
            Call GetStartupInfo(strServer    , _
                              strOutputFile , _
                              strUserName   , _
                              strPassword     )
            Call LdOrderGrp(strServer    , _
                           strOutputFile , _
                           strUserName   , _
                           strPassword     )
            Call ListJobs(strServer       , _
                          strOutputFile   , _
                          strUserName     , _
                          strPassword     , _
                          intSortOrder    , _
                          intSortProperty , _
                          intWidth        , _
                          strProperties   , _
                          intWidths         )

' programgroups.vbs - lists program groups for all users on machine!

' eventlogmon.vbs - monitors event log activity for machines.
' fileman.vbs - file manager utility
' createusers.vbs - ADspath of user object container!
' listproperties.vbs - lists properties of AD objects!
' regconfig.vbs - outputs or modifies the registry configuration size!

' query.vbs /? - a generic WBEM query on namespace, class, property
' enumnamespaces.vbs - enumerate namespace on machine!
' enumclasses.vbs - enumerate classes on machine!
' enuminstances.vbs - enumerates instances of a WBEM class within a namespace!
' osreconfig.vbs /? - get or toggle the OS recovery configuration /T code for a machine!

' exec.vbs - executes or kills a command for a process on a server!

        Case CONST_ERROR
            'Do Nothing

        Case Else                    'Default -- should never happen
            Call Wscript.Echo("Error occurred in passing parameters.")

    End Select

'********************************************************************
'* End of Script
'********************************************************************

'********************************************************************
'*
'* Sub ListOS() - from listos.vbs
'*
'* Purpose: Lists properties of the operating system on a machine.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************
Private Sub ListOS(strServer, strOutputFile, strUserName, strPassword)



    ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, objOSSet, objOS
    Dim strWBEMClass

    strWBEMClass = "Win32_OperatingSystem"

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    'Get the set
    Set objOSSet = objService.InstancesOf(strWBEMClass)
    If blnErrorOccurred("Could not obtain " & strWBEMClass & " instance.") Then
        Exit Sub
    End If

    If objOSSet.Count = 0 Then
        Call WriteLine("No Operating System information is available.", _
                        objOutputFile)    
        Exit Sub
    End If

    Call WriteLine("Operating System information for Machine " & _
                    strServer, objOutputFile)
    Call WriteLine("", objOutputFile)

    For Each objOS In objOSSet
        Call WriteLine("Name            = " & objOS.Caption, objOutputFile)
        Call WriteLine("Version         = " & objOS.Version , objOutputFile)
        Call WriteLine("Computer Name   = " & objOS.CSName, objOutputFile)
        Call WriteLine("Registered User = " & objOS.RegisteredUser, _
                        objOutputFile)
        Call WriteLine("Manufacturer    = " & objOS.Manufacturer, _
                        objOutputFile)
        Call WriteLine("Install Date    = " & _
                        strFormatMOFTime(objOS.InstallDate)  , objOutputFile)
        Call WriteLine("Last Bootup     = " & _
                        strFormatMOFTime(objOS.LastBootUpTime) , objOutputFile)
        Call WriteLine("", objOutputFile)
    Next

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If
End Sub

'********************************************************************
'*
'* Sub LstCompSys() - from compsys.vbs
'*
'* Purpose: Outputs Information on CDRomDrives.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************

Private Sub LstCompSys(strServer, strOutputFile, strUserName, strPassword)

    ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, objCompSet, objComp
    Dim strLine, strWbemClass

    strWBEMClass = "Win32_ComputerSystem"

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    'Get the set
    Set objCompSet = objService.InstancesOf(strWBEMClass)
    If blnErrorOccurred("Could not obtain " & strWBEMClass & " instance.") Then
        Exit Sub
    End If

    If objCompSet.Count = 0 Then
        Call WriteLine("No computer system information is available." _
                     , objOutputFile)    
        Exit Sub
    End If

    Call WriteLine("Computer system information for Machine " & strServer, _
                    objOutputFile)
    Call WriteLine("", objOutputFile)

    For Each objComp In objCompSet
        Call WriteLine("Name                         = " _
                     & objComp.Name, objOutputFile)
        Call WriteLine("Domain                       = " _
                     & objComp.Domain, objOutputFile)
        Call WriteLine("Infrared Supported           = " _
                     & objComp.InfraredSupported, objOutputFile)
        Call WriteLine("Network Server Mode Enabled  = " _
                     & objComp.NetworkServerModeEnabled, objOutputFile)
        Call WriteLine("System Type                  = " _
                     & objComp.SystemType, objOutputFile)
        Call WriteLine("Primary Owner Name           = " _
                     & objComp.PrimaryOwnerName, objOutputFile)
        Call WriteLine("", objOutputFile)
    Next

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub GetProcInfo() - from processor.vbs
'*
'* Purpose: Gets CPU information for a machine.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************
Private Sub GetProcInfo(strServer, strOutputFile, strUserName, strPassword)

    ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, objProcSet, objProc
    Dim strWBEMClass

    strWBEMClass = "Win32_Processor"

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    'Get the set
    Set objProcSet = objService.InstancesOf(strWBEMClass)
        If blnErrorOccurred("Could not obtain " & _
                   strWBEMClass & " instance.") Then
        Exit Sub
    End If

    If objProcSet.Count = 0 Then
        Call WriteLine("No processor information is available.", objOutputFile)    
        Exit Sub
    End If

    Call WriteLine("Processor information for Machine " _
                  & strServer, objOutputFile)
    Call WriteLine("", objOutputFile)

    For Each objProc In objProcSet
        Call WriteLine("Name                = " & _
             objProc.Name, objOutputFile)
        Call WriteLine("Current Voltage     = " & _
             objProc.CurrentVoltage, objOutputFile)
        Call WriteLine("Device ID           = " & _
             objProc.DeviceID, objOutputFile)
        Call WriteLine("Status              = " & _
             objProc.CpuStatus, objOutputFile)
        Call WriteLine("Data Width          = " & _
             objProc.DataWidth, objOutputFile)
        Call WriteLine("Current Clock Speed = " & _
             objProc.CurrentClockSpeed, objOutputFile)
        Call WriteLine("L2 Cache Size       = " & _
             objProc.L2CacheSize, objOutputFile)
        Call WriteLine("Level               = " & _
             objProc.Level, objOutputFile)
        Call WriteLine("External Clock      = " & _
             objProc.ExtClock, objOutputFile)
        Call WriteLine("", objOutputFile)
    Next

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub CheckBios - from checkbios.vbs
'*
'* Purpose: Displays information on system BIOS.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************
Private Sub CheckBios(strServer  ,  _
                      OutputFile ,  _
                      strUserName,  _
                      strPassword   )

    ON ERROR RESUME NEXT


    Dim objFileSystem, objOutputFile, objService, objSet, obj, objInst
    Dim strLine, strClass, strBiosCharacteristic

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    'Display Win32_Bios information
    strClass = "Win32_Bios"
    Set objSet = objService.InstancesOf(strClass)
    If blnErrorOccurred ("obtaining the "& strClass) Then Exit Sub
    WriteLine vbCRLF & CStr(objSet.Count) & " instance(s) of " & strClass & _
            " on " & strServer & ":", objOutputFile

    strBiosCharacteristic = Array( _
     "Reserved","Reserved","Unknown", _
     "BIOS Characteristics Not Supported",_
     "ISA is supported","MCA is supported","EISA is supported", _
     "PCI is supported", "PC Card (PCMCIA) is supported", _
     "Plug and Play is supported","APM is supported", _
     "BIOS is Upgradeable (Flash)","BIOS shadowing is allowed", _
     "VL-VESA is supported", "ESCD support is available", _
     "Boot from CD is supported","Selectable Boot is supported",_
     "BIOS ROM is socketed ","Boot From PC Card (PCMCIA) is supported",_
     "EDD (Enhanced Disk Drive) Specification is supported",_
     "Int 13h - Japanese Floppy for NEC 9800 1.2mb (3.5"", 1k Bytes/Sector," _
       & " 360 RPM) is supported",_
     "Int 13h - Japanese Floppy for Toshiba 1.2mb " _
       & "(3.5"", 360 RPM) is supported",_
     "Int 13h - 5.25"" / 360 KB Floppy Services are supported",_
     "Int 13h - 5.25"" /1.2MB Floppy Services are supported",_
     "Int 13h - 3.5"" / 720 KB Floppy Services are  supported",_
     "Int 13h - 3.5"" / 2.88 MB Floppy Services are supported",_
     "Int 5h, Print Screen Service is supported", _
     "Int 9h, 8042 Keyboard services are supported",_
     "Int 14h, Serial Services are supported", _
     "Int 17h, Printer Services are supported",_
     "Int 10h, CGA/Mono Video Services are supported", _
     "NEC PC-98","ACPI supported",_
     "USB Legacy is supported","AGP is supported","I2O boot is supported",_
     "LS-120 boot is supported","ATAPI ZIP Drive boot is supported", _
     "1394 boot is supported", "Smart Battery supported"_
    )
  
    For Each obj In objSet

        WriteLine Replace (Space(79), " ", "-"), objOutputFile

        'Win32_Bios Properties
        WriteLine "Version:               "& obj.Version                      _
                   , objOutputFile
        WriteLine "ReleaseDate:           "& strFormatMOFTime(obj.ReleaseDate)_
                   , objOutputFile
        WriteLine "Name:                  "& obj.Name                         _
                   , objOutputFile
        WriteLine "PrimaryBIOS:           "& strYesOrNo (obj.PrimaryBIOS)     _
                   , objOutputFile
        WriteLine "SoftwareElementState:  "& CStr(obj.SoftwareElementState)   _
                   , objOutputFile
        WriteLine "TargetOperatingSystem: "& CStr(obj.TargetOperatingSystem)  _
                   , objOutputFile
        WriteLine "OtherTargetOS:         "& obj.OtherTargetOS                _
                   , objOutputFile
        WriteLine "Status:                "& obj.Status                       _
                   , objOutputFile

        UBound(obj.BiosCharacteristics)
        If Err.Number Then
            Err.Clear
        Else

            WriteLine"BiosCharacteristics:", objOutputFile

            For i = 0 to UBound (obj.BiosCharacteristics)
                WriteLine Space(2) & strBiosCharacteristic _
                (obj.BiosCharacteristics(i)), objOutputFile
            Next 

        End If

    Next

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub LstDMAs() - from dmachan.vbs
'*
'* Purpose: Get the bus information for a machine.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************
Private Sub LstDMAs(strServer, strOutputFile, strUserName, strPassword)



    ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, objDMASet, objDMA
    Dim strWBEMClass

    strWBEMClass = "Win32_DMAChannel"

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    'Get the set
    Set objDMASet = objService.InstancesOf(strWBEMClass)
    If blnErrorOccurred("Could not obtain " & strWBEMClass & " instance.") Then
        Exit Sub
    End If

    If objDMASet.Count = 0 Then
        Call WriteLine("No DMA channel information is available.", _
                        objOutputFile)    
        Exit Sub
    End If

    Call WriteLine("DMA channel information for Machine " & _
                    strServer, objOutputFile)
    Call WriteLine("", objOutputFile)

    For Each objDMA In objDMASet
        Call WriteLine("Name   = " & objDMA.Name, objOutputFile)
        Call WriteLine("Port   = " & objDMA.Port, objOutputFile)
        Call WriteLine("Status = " & objDMA.Status, objOutputFile)
        Call WriteLine("", objOutputFile)
    Next

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If
End Sub

'********************************************************************
'*
'* Sub LstIRQs() - from irqres.vbs
'*
'* Purpose: List the desktop properties on a system.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************
Private Sub LstIRQs(strServer, strOutputFile, strUserName, strPassword)


    ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, objIRQSet, objIRQ, objInst
    Dim strWBEMClass

    strWBEMClass = "Win32_IRQResource"

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    'Get the set
    Set objIRQSet = objService.InstancesOf(strWBEMClass)
    If blnErrorOccurred("Could not obtain " & strWBEMClass & " instance.") Then
        Exit Sub
    End If

    If objIRQSet.Count = 0 Then
        Call WriteLine("No IRQ resource information is available.", objOutputFile)    
        Exit Sub
    End If

    Call WriteLine("IRQ resource information for Machine " & strServer, objOutputFile)
    Call WriteLine("", objOutputFile)

    For Each objIRQ In objIRQSet
        Call WriteLine("Name = " & objIRQ.Name, objOutputFile)
        Call WriteLine("Number = " & objIRQ.IRQNumber, objOutputFile)
        Call WriteLine("Hardware = " & objIRQ.Hardware, objOutputFile)
        Call WriteLine("", objOutputFile)
    Next

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub GetSndDevInfo()
'*
'* Purpose: Get the sound device configuration of a machine.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************
Private Sub GetSndDevInfo(strServer, strOutputFile, strUserName, strPassword)

    ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, objSndSet, objSnd
    Dim strWBEMClass

    strWBEMClass = "Win32_SoundDevice"

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    'Get the set
    Set objSndSet = objService.InstancesOf(strWBEMClass)
    If blnErrorOccurred("Could not obtain " & strWBEMClass & " instance.") Then
        Exit Sub
    End If

    If objSndSet.Count = 0 Then
        Call WriteLine("No sound device information is available.", _
            objOutputFile)    
        Exit Sub
    End If

    Call WriteLine("Sound device configuration information for Machine " & _
        strServer, objOutputFile)
    Call WriteLine("", objOutputFile)

    For Each objSnd In objSndSet
        Call WriteLine("Name          = " & objSnd.ProductName, objOutputFile)
        Call WriteLine("PNP Device ID = " & objSnd.PNPDeviceID, objOutputFile)
        Call WriteLine("Status        = " & objSnd.Status, objOutputFile)
        Call WriteLine("", objOutputFile)
    Next

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub LstSerialInfo() - from serialport.vbs
'*
'* Purpose: Outputs the serial port configurations.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************
Private Sub LstSerialInfo(strServer, strOutputFile, strUserName, strPassword)

    ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, objSerialSet, objSerial
    Dim strWBEMClass

    strWBEMClass = "Win32_SerialPortConfiguration"

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    'Get the set
    Set objSerialSet = objService.InstancesOf(strWBEMClass)
    If blnErrorOccurred("Could not obtain " & strWBEMClass & " instance.") Then
        Exit Sub
    End If

    If objSerialSet.Count = 0 Then
        Call WriteLine("No serial port information is available.", _
          objOutputFile)    
        Exit Sub
    End If

    Call WriteLine("Serial port configurations for Machine " & _
                    strServer, objOutputFile)
    Call WriteLine("", objOutputFile)

    For Each objSerial In objSerialSet
        Call WriteLine("Name          = " & objSerial.Name, objOutputFile)
        Call WriteLine("Parity        = " & objSerial.Parity, objOutputFile)
        Call WriteLine("Stop Bits     = " & objSerial.StopBits, objOutputFile)
        Call WriteLine("Bits Per Byte = " & objSerial.BitsPerByte,objOutputFile)
        Call WriteLine("Baud Rate     = " & objSerial.BaudRate, objOutputFile)
        Call WriteLine("", objOutputFile)
    Next

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub GetCacheInfo() - from cacheinfo.vbs
'* Purpose: Get the cache information for a machine.
'* Input:   strServer           a machine name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*          strOutputFile       an output file name
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************
Private Sub GetCacheInfo(strServer, strOutputFile, strUserName, strPassword)

    ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, objCacheSet, objCache
    Dim strQuery, strMessage
    Dim strWBEMClass, strErrorCorrectType, strLocation

    strWBEMClass = "Win32_CacheMemory"

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    'Get the set
    Set objCacheSet = objService.InstancesOf(strWBEMClass)
    If blnErrorOccurred("Could not obtain " & strWBEMClass & " instance.") Then
        Exit Sub
    End If

    If objCacheSet.Count = 0 Then
        Call WriteLine("No cache information is available.", objOutputFile)    
        Exit Sub
    End If

    Call WriteLine("Cache information for Machine " & strServer, objOutputFile)
    Call WriteLine("", objOutputFile)

    For Each objCache In objCacheSet
        Select Case objCache.ErrorCorrectType
            Case 0
                strErrorCorrectType = "(0) Reserved"
            Case 1
                strErrorCorrectType = "(1) Other"
            Case 2
                strErrorCorrectType = "(2) Unknown"
            Case 3
                strErrorCorrectType = "(3) None"
            Case 4
                strErrorCorrectType = "(4) Parity"
            Case 5
                strErrorCorrectType = "(5) Single-bit ECC"
            Case 6
                strErrorCorrectType = "(6) Multi-bit ECC"
        End Select
        Select Case objCache.Location
            Case 0
                strLocation = "(0) Internal"
            Case 1
                strLocation = "(1) External"
            Case 2
                strLocation = "(2) Reserved"
            Case 3
                strLocation = "(3) Unknown"
        End Select 

        Call WriteLine("Name               = " _
                      & objCache.Name, objOutputFile)
        Call WriteLine("Error Correct Type = " _
                      & strErrorCorrectType, objOutputFile)
        Call WriteLine("Location           = " _
                      & strLocation, objOutputFile)
        Call WriteLine("Max Cache Size     = " _
                      & objCache.MaxCacheSize, objOutputFile)
        Call WriteLine("", objOutputFile)
    Next

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub CdRomDrives()
'*
'* Purpose: Outputs Information on CDRomDrives.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************
Private Sub CdRomDrives(strServer, strOutputFile, strUserName, strPassword)

    ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, objSet, obj, objInst
    Dim strLine

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If


    'Get the first instance
    Set objSet = objService.InstancesOf("Win32_CDRomDrive")
    If blnErrorOccurred ("obtaining the Win32_CDRomDrive.") Then Exit Sub

    if objSet.Count = 0 then
        WriteLine "There are no CD-Rom drives detected.", objOutputFile
        Exit Sub
    End If
    For Each objInst In objSet

        WriteLine vbCRLF & _
           objInst.Manufacturer& " "& _
           objInst.Description& ", drive "& _
           objInst.Drive& " on "& _
           objInst.SystemName, objOutputFile
        WriteLine "Status:                     "& objInst.Status, objOutputFile
        WriteLine "Media Loaded:               "& _
                   strYesOrNo(objInst.MediaLoaded), objOutputFile
        if objInst.MediaLoaded Then
            WriteLine "Volume Name:                "& CStr(objInst.VolumeName)_
                                                    , objOutputFile
            WriteLine "Volume Serial Number:       "& _
                       CStr(objInst.VolumeSerialNumber), objOutputFile
            WriteLine "Maximum Component Length:   "& _
                       objInst.MaximumComponentLength, objOutputFile
        End If
        WriteLine "Device ID:                  "& objInst.DeviceID _
                                                , objOutputFile
        If len(objInst.PNPDeviceID) < 45 then
             WriteLine "PNP Device ID:              "& objInst.PNPDeviceID _
                                                     , objOutputFile
        End If
        WriteLine "SCSI Target Id:             "& objInst.SCSITargetId _
                                                , objOutputFile
        WriteLine "Drive Integrity:            "& objInst.DriveIntegrity _
                                                , objOutputFile
        WriteLine "Config Manager Error Code:  "& _
                   objInst.ConfigManagerErrorCode, objOutputFile
        WriteLine "Config Manager User Config: "& _
                   objInst.ConfigManagerUserConfig, objOutputFile
        WriteLine "Creation Class Name:        "& objInst.CreationClassName _
                                                , objOutputFile

    Next

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub GetBusInfo()
'*
'* Purpose: Get the bus information for a machine.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************
Private Sub GetBusInfo(strServer, strOutputFile, strUserName, strPassword)

    ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, objBusSet, objBus
    Dim strQuery, strMessage
    Dim strWBEMClass

    strWBEMClass = "Win32_Bus"

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    'Get the set
    Set objBusSet = objService.InstancesOf(strWBEMClass)
    If blnErrorOccurred("Could not obtain " & strWBEMClass _
                      & " instance.") Then
        Exit Sub
    End If

    If objBusSet.Count = 0 Then
        Call WriteLine("No bus information is available.", objOutputFile)    
        Exit Sub
    End If

    Call WriteLine("Bus information for Machine " & strServer, objOutputFile)
    Call WriteLine("", objOutputFile)

    For Each objBus In objBusSet
        Call WriteLine("Name = " & objBus.Name, objOutputFile)
        Call WriteLine("Bus Num = " & objBus.BusNum, objOutputFile)
        Call WriteLine("Bus Type = " & objBus.BusType, objOutputFile)
        Call WriteLine("Device ID = " & objBus.DeviceID, objOutputFile)
        Call WriteLine("PNP Device ID = " & objBus.PNPDeviceID, objOutputFile)
        Call WriteLine("", objOutputFile)
    Next

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub LstKeyBoard - from keyboard.vbs
'*
'* Purpose: Outputs Information on the Keyboard.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************
Private Sub LstKeyBoard(strServer, strOutputFile, strUserName, strPassword)

    ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objSet, obj, objInst, objService
    Dim strWBEMClass, strLin

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    'Get the first instance
    strWBEMClass = "Win32_Keyboard"
    Set objSet = objService.InstancesOf(strWBEMClass)
    If blnErrorOccurred ("obtaining the " & strWBEMClass) Then Exit Sub

    Wscript.Echo CStr(objSet.Count) & " instance(s) of " & strWBEMClass & _
               " on " & strServer & ":", objOutputFile

    For Each obj In objSet

        WriteLine Replace (Space(63), " ", "-"), objOutputFile

        If Not IsNull(obj.Description) Then
            WriteLine "Description:              " _
                    & obj.Description, objOutputFile
        Else
            WriteLine "DeviceID:                 "& obj.DeviceID,  objOutputFile
        End If

        WriteLine "Layout:                   "& obj.Layout,      objOutputFile
        WriteLine "NumberOfFunctionKeys:     "& _
                  CStr(obj.NumberOfFunctionKeys), objOutputFile
        WriteLine "PowerManagementSupported: "& _
                  strYesOrNo (obj.PowerManagementSupported), objOutputFile
        WriteLine "PNPDeviceID:              "& obj.PNPDeviceID, objOutputFile

    Next

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub GetPointDevInfo()
'*
'* Purpose: Get the pointing device information for a machine.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************
Private Sub GetPointDevInfo(strServer, strOutputFile, strUserName, strPassword)

    ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, objPntDevSet, objPntDev
    Dim strWBEMClass

    strWBEMClass = "Win32_PointingDevice"

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    'Get the set
    Set objPntDevSet = objService.InstancesOf(strWBEMClass)
    If blnErrorOccurred("Could not obtain " & strWBEMClass & " instance.") Then
        Exit Sub
    End If

    If objPntDevSet.Count = 0 Then
        Call WriteLine("No pointing device information is available.", _
                        objOutputFile)    
        Exit Sub
    End If

    Call WriteLine("Pointing device information for Machine " & strServer, _
                       objOutputFile)
    Call WriteLine("", objOutputFile)

    For Each objPntDev In objPntDevSet
        Call WriteLine("Name = " & objPntDev.Name, objOutputFile)
        Call WriteLine("Inf File Name = " & objPntDev.InfFileName, _
                        objOutputFile)
        Call WriteLine("Inf Section = " & objPntDev.InfSection, _
                        objOutputFile)
        Call WriteLine("Hardware Type = " & objPntDev.HardwareType, _
                        objOutputFile)
        Call WriteLine("Device ID= " & objPntDev.DeviceID, _
                        objOutputFile)
        Call WriteLine("Number Of Buttons= " & objPntDev.NumberOfButtons, _
                        objOutputFile)
        Call WriteLine("PNP Device ID = " & objPntDev.PNPDeviceID, _
                        objOutputFile)
        Call WriteLine("Status = " & objPntDev.Status, _
                        objOutputFile)
        Call WriteLine("", objOutputFile)
    Next
  
    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub LstDpConInfo() - from lstdpconinfo.vbs
'*
'* Purpose: Obtains the display controllor information of a machine.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************
Private Sub LstDpConInfo(strServer, strOutputFile, strUserName, strPassword)



    ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, objDisSet, objDis
    Dim strQuery, strMessage, strCat

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    'Get the display configuration
    Set objDisSet = objService.InstancesOf _
                   ("Win32_DisplayControllerConfiguration")
    If Err.Number Then
      Print "Error 0x" & CStr(Hex(Err.Number)) & _
            " occurred getting the memory configuration."
      If Err.Description <> "" Then
          Print "Error description: " & Err.Description & "."
      End If
      Err.Clear
      Exit Sub
    End If

    Dim intGetFirst
    intGetFirst = 0
    Dim objInst

    For Each objInst in objDisSet
      If intGetFirst = 0 Then Set objDis = objInst
      intGetFirst = intGetFirst + 1
    Next

    If IsEmpty(strServer) Then
      Dim objWshNet

      Set objWshNet = CreateObject("Wscript.Network")
      strServer = objWshNet.ComputerName
    End If

    Call WriteLine("Display Controllor Information for Machine " & _
                    strServer, objOutputFile)

    Call WriteLine("Discription       = " & _
                    objDis.Description,         objOutputFile)
    Call WriteLine("Bits Per Pixel    = " & _
                    objDis.BitsPerPixel,        objOutputFile)
    Call WriteLine("Color Planes      = " & _
                    objDis.ColorPlanes,         objOutputFile)
    Call WriteLine("Refresh Rate      = " & _
                    objDis.RefreshRate    ,     objOutputFile)
    Call WriteLine("Vertical Res      = " & _
                    objDis.VerticalResolution,  objOutputFile)
    Call WriteLine("Video Mode        = " & _
                    objDis.VideoMode,           objOutputFile)

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If
End Sub

'********************************************************************
'*
'* Sub GetDispInfo() - from listdisplayconfig.vbs
'* Purpose: Obtains the display configuration of a machine.
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************
Private Sub GetDispInfo(strServer     , _
                        strOutputFile , _
                        strUserName   , _
                        strPassword     )

    ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, objWshNet
    Dim objInst,  objDisSet, objDis
    Dim strQuery, strMessage, strCat
    Dim intGetFirst

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    'Get the display configuration
    Set objDisSet = objService.InstancesOf("Win32_DisplayConfiguration")
    If Err.Number Then
        Wscript.Echo("Error 0x" & CStr(Hex(Err.Number)) _
                   & " occurred getting the memory configuration.")
        If Err.Description <> "" Then
            Wscript.Echo("Error description: " & Err.Description & ".")
        End If
        Err.Clear
        Exit Sub
    End If

    intGetFirst = 0

    For Each objInst in objDisSet
        If intGetFirst = 0 Then Set objDis = objInst
            intGetFirst = intGetFirst + 1
        Next

    If IsEmpty(strServer) Then
        Set objWshNet = CreateObject("Wscript.Network")
            strServer = objWshNet.ComputerName
    End If

    Call WriteLine("Display Configuration for Machine " & _
                    strServer, objOutputFile)
    Call WriteLine("Device Name       = " & objDis.DeviceName, _
                    objOutputFile)
    Call WriteLine("Driver Version    = " & objDis.DriverVersion, _
                    objOutputFile)
    Call WriteLine("Display Frequency = " & objDis.DisplayFrequency, _
                    objOutputFile)
    Call WriteLine("Pixel Width       = " & objDis.PelsWidth, _
                    objOutputFile)
    Call WriteLine("Pixel Height      = " & objDis.PelsHeight, _
                    objOutputFile)
    Call WriteLine("Bits Per Pixel    = " & objDis.BitsPerPel, _
                    objOutputFile)

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub GetPPortInfo() - from parallelport.vbs
'*
'* Purpose: Get the parallel port information for a machine.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************
Private Sub GetPPortInfo(strServer, strOutputFile, strUserName, strPassword)

    ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, objPPortSet, objPPort
    Dim strWBEMClass

    strWBEMClass = "Win32_ParallelPort"

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    'Get the set
    Set objPPortSet = objService.InstancesOf(strWBEMClass)
    If blnErrorOccurred("Could not obtain " & strWBEMClass & " instance.") Then
        Exit Sub
    End If

    If objPPortSet.Count = 0 Then
        Call WriteLine("No parallel port information is available.", _
                        objOutputFile)    
        Exit Sub
    End If

    Call WriteLine("Parallel port information for Machine " & _
                    strServer, objOutputFile)
    Call WriteLine("", objOutputFile)

    For Each objPPort In objPPortSet
        Call WriteLine("Name = " & objPPort.Name, objOutputFile)
        Call WriteLine("PNP Device ID = " & objPPort.PNPDeviceID, objOutputFile)
        Call WriteLine("Device ID = " & objPPort.DeviceID, objOutputFile)
        Call WriteLine("", objOutputFile)
    Next

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub Drives()
'*
'* Purpose: List the desktop properties on a system.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************
Private Sub Drives(strServer, strOutputFile, strUserName, strPassword)

    ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, objset, objInst
    Dim strLine

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    'Get the first instance
    Set objSet = objService.InstancesOf("Win32_DiskDrive")
    If blnErrorOccurred ("obtaining the Win32_DiskDrive.") Then Exit Sub

    For Each objInst In objSet
        WriteLine "", objOutputFile
        WriteLine objInst.Caption& ":   "& objInst.Description, objOutputFile
        WriteLine "Status:             "& objInst.Status, objOutputFile
        WriteLine "Media Loaded:       "& strYesOrNo(objInst.MediaLoaded), _
                   objOutputFile
        WriteLine "Partitions:         "& CStr(objInst.Partitions), _
                   objOutputFile
        WriteLine "System Name:        "& objInst.SystemName, objOutputFile

        if objInst.InterfaceType = "SCSI" Then
            WriteLine "SCSIBus:            "& CStr(objInst.SCSIBus), _
                      objOutputFile
            WriteLine "SCSILogicalUnit:    "& CStr(objInst.SCSILogicalUnit), _
                      objOutputFile
            WriteLine "SCSIPort:           "& CStr(objInst.SCSIPort), _
                      objOutputFile
            WriteLine "SCSITargetId:       "& CStr(objInst.SCSITargetId), _
                      objOutputFile
        End If

        WriteLine "Manufacturer/Model: "& _
             objInst.Manufacturer& " "& objInst.Model, objOutputFile
        WriteLine "Size:               "& _
             strInsertCommas(objInst.Size), objOutputFile
        WriteLine "Total Cylinders:    "& _
             strInsertCommas(objInst.TotalCylinders), objOutputFile
        WriteLine "Total Heads    :    "& _
             strInsertCommas(CStr(objInst.TotalHeads)), objOutputFile
        WriteLine "Total Sectors:      "& _
             strInsertCommas(objInst.TotalSectors), objOutputFile
        WriteLine "Total Tracks:       "& _
             strInsertCommas(objInst.TotalTracks), objOutputFile
        WriteLine "Sectors Per Track:  "& _
             strInsertCommas(objInst.SectorsPerTrack), objOutputFile
        WriteLine "Tracks Per Cylinder:"& _
             strInsertCommas(CStr(objInst.TracksPerCylinder)), objOutputFile
        WriteLine "Bytes Per Sector:   "& _
             strInsertCommas(objInst.BytesPerSector), objOutputFile
        WriteLine "Name:               "& _
             objInst.Name, objOutputFile
        WriteLine "Creation Class Name:"& _
             objInst.CreationClassName, objOutputFile

    Next

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub GetSCSIs() - from scsicontroller.vbs
'*
'* Purpose: Get the SCSI controller information on a machine.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************
Private Sub GetSCSIs(strServer, strOutputFile, strUserName, strPassword)

    ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, objSCSISet, objSCSI, objInst
    Dim strWBEMClass

    strWBEMClass = "Win32_SCSIController"

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    'Get the set
    Set objSCSISet = objService.InstancesOf(strWBEMClass)
    If blnErrorOccurred("Could not obtain " & strWBEMClass & " instance.") Then
        Exit Sub
    End If

    If objSCSISet.Count = 0 Then
        Call WriteLine("No SCSI information is available.", objOutputFile)    
        Exit Sub
    End If

    Call WriteLine("SCSI information for Machine " & strServer, objOutputFile)
    Call WriteLine("", objOutputFile)

    For Each objSCSI In objSCSISet
        Call WriteLine("Name          = " & objSCSI.Name, objOutputFile)
        Call WriteLine("Driver Name   = " & objSCSI.DriverName, objOutputFile)
        Call WriteLine("PNP Device ID = " & objSCSI.PNPDeviceID, objOutputFile)
        Call WriteLine("Device Map    = " & objSCSI.DeviceMap, objOutputFile)
        Call WriteLine("", objOutputFile)
    Next

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub GetDskPartInf()
'*
'* Purpose: Get the disk partition information of a machine.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************

Private Sub GetDskPartInf(strServer     , _
                          strOutputFile , _
                          strUserName   , _
                          strPassword     )


    ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, objDSKPSet, objDSKP
    Dim strQuery, strMessage
    Dim strWBEMClass

    strWBEMClass = "Win32_DiskPartition"

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    'Get the set
    Set objDSKPSet = objService.InstancesOf(strWBEMClass)
        If blnErrorOccurred("Could not obtain " & strWBEMClass & _
            " instance.") Then
        Exit Sub
    End If

    If objDSKPSet.Count = 0 Then
        Call WriteLine("No disk partition information is available.", _
            objOutputFile)    
        Exit Sub
    End If

    Call WriteLine("Disk partition information for Machine " & strServer, _
            objOutputFile)
    Call WriteLine("", objOutputFile)

    For Each objDSKP In objDSKPSet
        Call WriteLine("Name              = " & objDSKP.Name, _
             objOutputFile)
        Call WriteLine("Device ID         = " & objDSKP.DeviceID, _
             objOutputFile)
        Call WriteLine("Size              = " & strInsertCommas(objDSKP.Size), _
             objOutputFile)
        Call WriteLine("Boot Partition    = " & objDSKP.BootPartition, _
             objOutputFile)
        Call WriteLine("Disk Index        = " & objDSKP.DiskIndex, _
             objOutputFile)
        Call WriteLine("Primary Partition = " & objDSKP.PrimaryPartition, _
             objOutputFile)
        Call WriteLine("Starting Offset   = " & objDSKP.StartingOffset, _
             objOutputFile)
        Call WriteLine("Type              = " & objDSKP.Type, _
             objOutputFile)
        Call WriteLine("", objOutputFile)
    Next

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub GetTapeDrive() - from tapedrive.vbs
'*
'* Purpose: Outputs Information on Disk Drives.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************
Private Sub GetTapeDrive(strServer, strOutputFile, strUserName, strPassword)

    'ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, objSet, obj
    Dim strWBEMClass, n

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    strWBEMClass = "Win32_TapeDrive"
      
    'Get the first instance
    Set objSet = objService.InstancesOf(strWBEMClass)
    If blnErrorOccurred ("obtaining the "& strWBEMClass) Then Exit Sub

    n = objSet.Count
    Call WriteLine("There are " & n & " tape drive(s) available.", _
      objOutputFile)
    Call WriteLine("", objOutputFile)
    If n <> 0 then
        For Each obj In objSet
            Call WriteLine("Name                  : " & obj.Name, _
              objOutputFile)
            Call WriteLine("Availability          : " & obj.Availability, _
              objOutputFile)
            Call WriteLine("Status                : " & obj.Status, _
              objOutputFile)
            Call WriteLine("Features Low          : " & obj.FeaturesLow, _
              objOutputFile)
            Call WriteLine("Features High         : " & obj.FeaturesHigh, _
              objOutputFile)
            Call WriteLine("Min Block Size        : " & _
              strInsertCommas(obj.MinBlockSize), objOutputFile)
            Call WriteLine("Max Block Size        : " & _
              strInsertCommas(obj.MaxBlockSize), objOutputFile)
            Call WriteLine("Default Block Size    : " & _
              strInsertCommas(obj.DefaultBlockSize), objOutputFile)
            Call WriteLine("Max Partition Count   : " & _
              obj.MaxPartitionCount, objOutputFile)
            Call WriteLine("EOT Warning Zone Size : " & _
              obj.EOTWarningZoneSize, objOutputFile)
            Call WriteLine("Padding               : " & _
              obj.Padding, objOutputFile)
            Call WriteLine("Report Set Marks      : " & _
              obj.ReportSetMarks, objOutputFile)
            Call WriteLine("Compression           : " & _
              obj.Compression, objOutputFile)
            Call WriteLine("ECC                   : " & _
              obj.ECC, objOutputFile)
            Call WriteLine("", objOutputFile)
        Next
    End If

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub ListSpace()
'* Purpose: Lists the size of each drive on a machine.
'* Input:   strServer     a machine name
'*          strUserName   the current user's name
'*          strPassword   the current user's password
'*          strOutputFile an output file name
'* Output:  Results of the search are either printed on screen or saved 
'*          in strOutputFile.
'*
'********************************************************************

Private Sub ListSpace(strServer, strUserName, strPassword, strOutputFile)

    ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, objEnumerator, objInstance
    Dim strQuery, strMessage
    Dim lngSpace
    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    strQuery = "Select Size, DeviceID From Win32_LogicalDisk"

    Set objEnumerator = objService.ExecQuery(strQuery,,0)
    If Err.Number Then
        Print "Error 0x" & CStr(Hex(Err.Number)) & " occurred during the query."
        If Err.Description <> "" Then
            Print "Error description: " & Err.Description & "."
        End If
        Err.Clear
        Exit Sub
    End If

    For Each objInstance in objEnumerator
        If Not (objInstance is nothing) Then
            strMessage = Space(2) & strPackString(objInstance.DeviceID, 5, 1, 0)
            lngSpace = objInstance.Size
            If lngSpace <> 0 Then
                strMessage = strMessage & _
                             strPackString(strInsertCommas(lngSpace), _
                             15, 0, 0 ) & " bytes"
            Else
                strMessage = strMessage & strPackString("not available", _
                             15, 0, 0)
            End If
            WriteLine strMessage, objOutputFile
        End If
        If Err.Number Then
            Err.Clear
        End If
    Next

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub ListFreeSpace()
'*
'* Purpose: Lists available disk space on all drives of a machine.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************
Private Sub ListFreeSpace(strServer, strOutputFile, strUserName, strPassword)

    ON ERROR RESUME NEXT
    Dim objFileSystem, objOutputFile, objService, objEnumerator, objInstance
    Dim strQuery, strMessage
    Dim lngSpace
    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    strQuery = "Select FreeSpace, DeviceID From Win32_LogicalDisk"

    Set objEnumerator = objService.ExecQuery(strQuery,,0)
    If Err.Number Then
        Wscript.Echo("Error 0x" & CStr(Hex(Err.Number)) & _
                     " occurred during the query.")
        If Err.Description <> "" Then
            Wscript.Echo("Error description: " & Err.Description & ".")
        End If
        Err.Clear
        Exit Sub
    End If

    For Each objInstance in objEnumerator
        If Not (objInstance is nothing) Then
            strMessage = Space(2) & strPackString(objInstance.DeviceID, 5, 1, 0)
            lngSpace = objInstance.FreeSpace
            If lngSpace <> 0 Then
                strMessage = strMessage & _
                  strPackString(strInsertCommas(lngSpace), 15, 0, 0 ) & " bytes"
            Else
                strMessage = strMessage & _
                  strPackString("not available", 15, 0, 0)
            End If
            WriteLine strMessage, objOutputFile
        End If
        If Err.Number Then
            Err.Clear
        End If
    Next

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub BootConfig
'*
'* Purpose: Get the Boot Configuration information for a machine.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************

Private Sub BootConfig (strServer, strOutputFile, strUserName, strPassword)

   ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, objSet, obj
    Dim strQuery, strMessage, strWBEMClass
    Dim i

    strWBEMClass = "Win32_BootConfiguration"

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2", _
                   strUserName, _
                   strPassword, _
                   strServer  , _ 
                   objService ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    'Get the first instance
    Set objSet = objService.InstancesOf(strWBEMClass )
    If blnErrorOccurred ("obtaining the "& strWBEMClass ) Then Exit Sub
    i = objSet.Count
    If blnErrorOccurred ("counting instances") Then Exit Sub
    WriteLine(vbCRLF & "The Following is the boot configuration for machine " _
             & strServer & ":" & vbCRLF), objOutputFile
    If blnErrorOccurred ("printing instance header") Then Exit Sub

    For Each obj In objSet
        WriteLine "Boot Directory     : " & strLimitStringLengthSlashes _ 
                                           (obj.BootDirectory,     58, 21) _
                                           , objOutputFile
        WriteLine "Configuration Path : " & strLimitStringLengthSlashes _
                                           (obj.ConfigurationPath, 58, 21) _
                                           , objOutputFile
        WriteLine "Scratch Directory  : " & strLimitStringLengthSlashes _
                                           (obj.ScratchDirectory,  58, 21) _
                                           , objOutputFile
        WriteLine "Temp Directory     : " & strLimitStringLengthSlashes _ 
                                           (obj.TempDirectory,     58, 21) _
                                           , objOutputFile

    Next

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub CodecFiles
'*
'* Purpose: Outputs Information on Codec Files.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*          blnDetails          Extra information to be displayed
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************
Private Sub CodecFiles(strServer  ,  _
                       OutputFile ,  _
                       strUserName,  _
                       strPassword,  _
                       blnDetails    )
                       
    ON ERROR RESUME NEXT


    Dim objFileSystem, objOutputFile, objService, objSet, obj
    Dim strLine, strClass
    Dim n
    Dim intWidth(16)

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    strClass = "Win32_CodecFile"

    'Get the first instance
    Set objSet = objService.InstancesOf(strClass)
    If blnErrorOccurred ("obtaining the "& strClass) Then Exit Sub

    i = objSet.Count
    WriteLine vbCRLF & CStr(i) & " instance" & strIf(i<>1,"s","") & " of " _
        & strClass& " on " & strServer & strIf(i>0,":","."), objOutputFile

    for i = 0 to 15: intWidth(i) = 4: next 'default widths
    intWidth(5) = 10 'FileSize
    intWidth(8) = 7 'Status

    'find column widths, ignoring headers & using 2 spacers
    for i = 0 to 15: intWidth(i) = intWidth(i) - 2: next
    For Each obj In objSet
        n = Len (obj.Name        ): if intWidth( 0) < n then intWidth( 0) = n
        n = Len (obj.Group       ): if intWidth( 1) < n then intWidth( 1) = n
        n = Len (obj.FileType    ): if intWidth( 2) < n then intWidth( 2) = n
        n = Len (obj.Version     ): if intWidth( 4) < n then intWidth( 3) = n
        n = Len (obj.Manufacturer): if intWidth( 3) < n then intWidth( 4) = n
        n = Len (obj.FileSize) + 1: if intWidth( 5) < n then intWidth( 5) = n
        n = Len (strFormatMOFTime(obj.CreationDate))
        if intWidth( 6) < n then intWidth( 6) = n
        n = Len (obj.Description ): if intWidth( 7) < n then intWidth( 7) = n
        n = Len (obj.Status      ): if intWidth( 8) < n then intWidth( 8) = n
        n = Len (obj.CSName      ): if intWidth(15) < n then intWidth(15) = n
    Next
    for i = 0 to 15: intWidth(i) = intWidth(i) + 2: next
    'print header
    strLine = Empty
    strLine = strLine + strPackString ("Name"        ,intWidth( 0),1,1)
    strLine = strLine + strPackString ("Group"       ,intWidth( 1),1,1)
    strLine = strLine + strPackString ("FileType    ",intWidth( 2),1,1)
    strLine = strLine + strPackString ("Version"     ,intWidth( 3),1,1)
    If blnDetails then
        strLine = strLine + strPackString ("Manufacturer", _
                  intWidth( 4),1,1)
        strLine = strLine + strPackString ("FileSize "   , _
                  intWidth( 5),0,1)
        strLine = strLine + strPackString ("Description ", _
                  intWidth( 7),1,1)
        strLine = strLine + strPackString ("Status      ", _
                  intWidth( 8),1,1)
        strLine = strLine + strPackString ("Readable    ", _
                  intWidth( 9),1,1)
        strLine = strLine + strPackString ("Writeable   ", _
                  intWidth(10),1,1)
        strLine = strLine + strPackString ("System      ", _
                  intWidth(11),1,1)
        strLine = strLine + strPackString ("Archive     ", _
                  intWidth(12),1,1)
        strLine = strLine + strPackString ("Hidden      ", _
                  intWidth(13),1,1)
        strLine = strLine + strPackString ("Encrypted   ", _
                  intWidth(14),1,1)
        strLine = strLine + strPackString ("CSName      ", _
                  intWidth(15),1,1)
    End If

    WriteLine " ", objOutputFile
    WriteLine strLine, objOutputFile

    'print header line
    n=0: for i = 0 to strIf(blnDetails,15,3): n = n + intWidth(i): next
    WriteLine Replace (Space(n), " ", "-"), objOutputFile
    'print records for each instance of class
    For Each obj In objSet
        strLine = Empty
        strLine = strLine + strPackString (obj.Name         ,intWidth( 0),1,1)
        strLine = strLine + strPackString (obj.Group        ,intWidth( 1),1,1)
        strLine = strLine + strPackString (obj.FileType     ,intWidth( 2),1,1)
        strLine = strLine + strPackString (obj.Version      ,intWidth( 3),1,1)
        if blnDetails then
            strLine = strLine + strPackString (obj.Manufacturer ,intWidth( 4),1,1)
            strLine = strLine + strPackString (obj.FileSize &" ",intWidth( 5),0,1)
            strLine = strLine + strPackString (obj.Description  ,intWidth( 7),1,1)
            strLine = strLine + strPackString (obj.Status       ,intWidth( 8),1,1)
            strLine = strLine + strPackString (strYesOrNo(obj.Readable) ,intWidth( 9),1,1)
            strLine = strLine + strPackString (strYesOrNo(obj.Writeable),intWidth(10),1,1)
            strLine = strLine + strPackString (strYesOrNo(obj.System)   ,intWidth(11),1,1)
            strLine = strLine + strPackString (strYesOrNo(obj.Archive)  ,intWidth(12),1,1)
            strLine = strLine + strPackString (strYesOrNo(obj.Hidden)   ,intWidth(13),1,1)
            strLine = strLine + strPackString (strYesOrNo(obj.Encrypted),intWidth(14),1,1)
            strLine = strLine + strPackString (obj.CSName       ,intWidth(15),1,1)
        end if

        WriteLine strLine, objOutputFile
    Next

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub ListAdapters() 
'* Purpose: Lists properties of all network adapters.
'* Input:   strServer           a machine name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************

Private Sub ListAdapters(strServer     , _
                         strOutputFile , _
                         strUserName   , _
                         strPassword     )


    ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, objEnumerator, objInstance
    Dim strProperties(3)
    Dim strQuery, strMessage, strTemp
    Dim i, j
    Dim blnEmpty

    strProperties(0) = "ServiceName"
    strProperties(1) = "DhcpEnabled"                    
    strProperties(2) = "Description"
    strProperties(3) = "IPAddress"

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    'Set the query string.
    strQuery = "Select "
    For i = 0 To UBound(strProperties)-1
        strQuery = strQuery & LCase(strProperties(i)) & ", "
    Next
    strQuery = strQuery & LCase(strProperties(i))
    strQuery = strQuery & " From Win32_NetworkAdapterConfiguration"

    blnEmpty = False

    Set objEnumerator = objService.ExecQuery(strQuery,,0)
    If Err.Number Then
        Print "Error 0x" & CStr(Hex(Err.Number)) & " occurred during the query."
        If Err.Description <> "" Then
            Print "Error description: " & Err.Description & "."
        End If
        Err.Clear
        Exit Sub
    End If

    For Each objInstance in objEnumerator
        For i = 0 To UBound(strProperties)
            strMessage = UCase(strPackString(strProperties(i), 30, 1, 0))
            strTemp = objInstance.properties_(strProperties(i))
            If Err.Number Then
                Err.Clear
            Else
                If IsArray(strTemp) Then
                    If strTemp(0) = "" Then
                        blnEmpty = True
                    Else
                        For j = 0 To UBound(strTemp)
                            strMessage = strMessage & strTemp(j) & ", "
                        Next
                        strMessage = Left(strMessage, Len(strMessage)-2)
                    End If
                Else
                    If IsNull(strTemp) Then
                        blnEmpty = True
                    Else
                        strTemp = CStr(strTemp)
                        If strTemp = "" Then
                            blnEmpty = True
                        Else
                            strMessage = strMessage & strTemp
                        End If
                    End If
                End If
                If blnEmpty Then
                    blnEmpty = False
                Else
                    WriteLine strMessage, objOutputFile
                End If
            End If            
        Next
        WriteLine "", objOutputFile
    Next


    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub GetBindings() - from protocolbinding.vbs
'*
'* Purpose: Lists Network Protocol Bindings on a machine.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************
Private Sub GetBindings(strServer, strOutputFile, strUserName, strPassword)

    ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, objSet, obj, obj1, obj2, obj3
    Dim strWBEMClass

    strWBEMClass = "Win32_ProtocolBinding"

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    'Get the first instance
    Set objSet = objService.InstancesOf(strWbemClass)
    If blnErrorOccurred ("obtaining the "& strWbemClass) Then Exit Sub

    'print records for each instance of class

    For Each obj In objSet
        strLine = Empty
        set obj1 = objService.Get(obj.Antecedent)
        set obj2 = objService.Get(obj.Device)
        set obj3 = objService.Get(obj.Dependent)
        Call WriteLine ("Protocol           : " & obj1.Name, objOutputFile)
        Call WriteLine ("Driver Description : " & obj3.Caption, objOutputFile)
        Call WriteLine ("Adapter            : " & obj2.Caption, objOutputFile)
        Call WriteLine ("Driver             : " & obj3.Name, objOutputFile)
        Call WriteLine ("StartMode          : " & obj3.StartMode, objOutputFile)
        Call WriteLine ("State              : " & obj3.State, objOutputFile)
        Call WriteLine ("Status             : " & obj3.Status, objOutputFile)                
        Call WriteLine ("", objOutputFile)
    Next

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub NetConnections() - from netconnections.vbs
'*
'* Purpose: Outputs Information on CDRomDrives.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************

Private Sub NetConnections(strServer, strOutputFile, strUserName, strPassword)

    ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, objNetSet, obj, objInst
    Dim strLine, strWbemClass

    strWbemClass = "Win32_SystemNetworkConnections"

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If


    'Get the set
    Set objNetSet = objService.InstancesOf(strWBEMClass)
    If blnErrorOccurred("Could not obtain " & strWBEMClass & " instance.") Then
        Exit Sub
    End If

    If objNetSet.Count = 0 Then
        Call WriteLine("No Network Connection information is available." _
                     , objOutputFile)    
        Exit Sub
    End If

    Call WriteLine("Network Connections for Machine " & strServer, _
                    objOutputFile)
    Call WriteLine("", objOutputFile)

    For Each objInst In objNetSet
        Set obj = objService.Get(objInst.PartComponent)
        Call WriteLine("Status        = " _
                     & Obj.Status, objOutputFile)
        Call WriteLine("Local Name    = " _
                     & Obj.LocalName, objOutputFile)
        Call WriteLine("Remote Name   = " _
                     & Obj.RemoteName, objOutputFile)
        Call WriteLine("Provider Name = " _
                     & Obj.ProviderName, objOutputFile)
        Call WriteLine("Persistent    = " _
                     & Obj.Persistent, objOutputFile)
        Call WriteLine("Resource Type = " _
                     & Obj.ResourceType, objOutputFile)
        Call WriteLine("", objOutputFile)
    Next

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub LstSysAccount() - from systemaccounts.vbs
'*
'* Purpose: Displays System Account Information.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************
Private Sub LstSysAccount(strServer, strOutputFile, strUserName, strPassword)

    ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, objSet, obj
    Dim strWBEMClass, strLine
    Dim j, m, n

    strWBEMClass = "Win32_SystemAccount"

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If
  
    'Get the first instance
    Set objSet = objService.InstancesOf(strWBEMClass)
    If blnErrorOccurred ("obtaining the "& strWBEMClass) Then Exit Sub

    For Each obj In objSet
        Call WriteLine("Caption  : " & obj.Caption, objOutputFile)
        Call WriteLine("SID      : " & obj.Sid, objOutputFile)
        Call WriteLine("SID Type : " & obj.SIDType, objOutputFile)
        Call WriteLine("Status   : " & obj.Status, objOutputFile)
        Call WriteLine("", objOutputFile)
    Next

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub UserAccounts() - from useraccounts.vbs
'*
'* Purpose: Displays User Account Information.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************
Private Sub UserAccounts(strServer     ,  _
                         strOutputFile ,  _
                         strUserName   ,  _
                         strPassword   ,  _
                         blnQuery      ,  _
                         strDomain     ,  _
                         strName       ,  _
                         blnLockout    ,  _
                         blnDisabled      )

    ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, objSet, obj
    Dim strWBEMClass
    Dim strLine, strSep, strQuery
    Dim H
    Dim strQueryArray(4)

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    strWBEMClass = "Win32_UserAccount"
  
    'Get the first instance
    If blnQuery = true  then
 
        I = 1
        If strDomain <> "" then
            strQueryArray(I) = "Domain = """ & strDomain & """"
            I = I + 1
        End If
        If strName <> "" then
            strQueryArray(I) = "Name = """ & strName & """"
            I = I + 1
        End If
        If blnLockout = true then
            strQueryArray(I) = "Lockout = True"
            I = I + 1
        End If
        If blnDisabled = true then
            strQueryArray(I) = "Disabled = True"
            I = I + 1
        End If
        I = I - 1
        strQuery = "Select * from Win32_UserAccount Where " & strQueryArray(1)
        If I > 1 then
            For H = 2 to I
                strQuery = strQuery & " and " & strQueryArray(H)
            Next
        End If
        Set objSet = objService.ExecQuery(strQuery,,0)
        If blnErrorOccurred ("obtaining the "& strWBEMClass) Then Exit Sub
    Else

        Set objSet = objService.ExecQuery("Select * from " & strWBEMClass,,0)
        If blnErrorOccurred ("obtaining the "& strWBEMClass) Then Exit Sub
    End If

    I = objSet.Count
    If I = 0 then
        Call WriteLine("There is no User Account Data available.", _
            objOutputFile)
    End If

    For Each obj In objSet
        WriteLine strSep, objOutputFile
        WriteLine "UserAccount : " & obj.Caption    , objOutputFile
        WriteLine "FullName    : " & obj.FullName   , objOutputFile
        WriteLine "Description : " & Left(obj.Description, 65), objOutputFile
        WriteLine "SID         : " & obj.SID        , objOutputFile
        WriteLine "AccountType : " & CStr(obj.AccountType), objOutputFile
        WriteLine "SIDType     : " & CStr(obj.SIDType)    , objOutputFile
        WriteLine "Status      : " & obj.Status           , objOutputFile
        WriteLine "Password      ", objOutputFile
        WriteLine "  Expires   : " & obj.PasswordExpires, objOutputFile
        WriteLine "  Changeable: " & obj.PasswordChangeable, objOutputFile        
        WriteLine "  Required  : " & obj.PasswordRequired, objOutputFile        
        WriteLine "Lockout     : " & obj.Lockout           , objOutputFile
        WriteLine "Disabled    : " & obj.Disabled           , objOutputFile
   
     Next

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub LstDeskTop() - from desktop.vbs
'*
'* Purpose: List the desktop properties on a system.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************
Private Sub LstDeskTop(strServer, strOutputFile, strUserName, strPassword)

    ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, objDeskSet, objDesk
    Dim strWBEMClass

    strWBEMClass = "Win32_DeskTop"

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    'Get the set
    Set objDeskSet = objService.InstancesOf(strWBEMClass)
    If blnErrorOccurred("Could not obtain " & _
                         strWBEMClass & " instance.") Then
        Exit Sub
    End If

    If objDeskSet.Count = 0 Then
        Call WriteLine("No desktop information is available.", objOutputFile)    
        Exit Sub
    End If

    Call WriteLine("Desktop information for Machine " & _
                    strServer, objOutputFile)
    Call WriteLine("", objOutputFile)

    For Each objDesk In objDeskSet
        Call WriteLine("Name                          = " & _
                        objDesk.Name, objOutputFile)
        Call WriteLine("Is Screen Saver Active?       = " & _
                        objDesk.ScreenSaverActive, objOutputFile)
        Call WriteLine("Is Screen Saver Security Set? = " & _
                        objDesk.ScreenSaverSecure, objOutputFile)
        Call WriteLine("Screen Saver Timeout          = " & _
                        objDesk.ScreenSaverTimeout & " seconds", objOutputFile)
        Call WriteLine("Cursor Blink Interval         = " & _
                        objDesk.CursorBlinkRate & _
                       " milliseconds", objOutputFile)
        Call WriteLine("", objOutputFile)
    Next

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub GetStartupInfo() - from startup.vbs
'*
'* Purpose: Enumerates the startup programs.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************
Private Sub GetStartupInfo(strServer, strOutputFile, strUserName, strPassword)

    ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, objStSet, objStart, objWshNet
    Dim strQuery, strMessage, strCat

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    'Get the startup commands
    Set objStSet = objService.InstancesOf("Win32_StartupCommand")
    If Err.Number Then
        Wscript.Echo("Error 0x" & CStr(Hex(Err.Number)) & _
            " occurred getting the startup instances.")
        If Err.Description <> "" Then
            Wscript.Echo("Error description: " & Err.Description & ".")
        End If
        Err.Clear
        Exit Sub
    End If

    If IsEmpty(strServer) Then
        Set objWshNet = CreateObject("Wscript.Network")
        strServer = objWshNet.ComputerName
    End If

    Call WriteLine("Startup commands on Machine " & strServer, objOutputFile)
    Call WriteLine("", objOutputFile)
    
    'Generate the header
    strMessage = Empty
    strMessage = strMessage + strPackString("User",     12, 1, 1)
    strMessage = strMessage + strPackString("Name",  20, 1, 1)
    strMessage = strMessage + strPackString("Command", 40, 1, 1)
    Call WriteLine(strMessage, objOutputFile)

    For Each objStart in objStSet
        strMessage = Empty

        strMessage = strMessage + strPackString(objStart.User,      12, 1, 1)
        strMessage = strMessage + strPackString(objStart.Caption,   20, 1, 1)
        strMessage = strMessage + strPackString(objStart.Command,   47, 1, 1)

        Call WriteLine(strMessage, objOutputFile)
    Next
    
    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub LdOrderGrp() - from ldordergrp.vbs
'*
'* Purpose: List the service dependency groups on a machine.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************
Private Sub LdOrderGrp(strServer, strOutputFile, strUserName, strPassword)

    ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, objGrpSet, objGrp
    Dim strWBEMClass, strMessage

    strWBEMClass = "Win32_LoadOrderGroup"

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    'Get the set
    Set objGrpSet = objService.InstancesOf(strWBEMClass)
    If blnErrorOccurred("Could not obtain " & strWBEMClass & " instance.") Then
        Exit Sub
    End If

    If objGrpSet.Count = 0 Then
        Call WriteLine("No service depency groups are available.", _
                        objOutputFile)    
        Exit Sub
    End If

    Call WriteLine("Service dependency groups for Machine " & strServer, _
                    objOutputFile)
    Call WriteLine("", objOutputFile)

    strMessage = strPackString("Group Name", 40, 1, 1)
    strMessage = strMessage + strPackString("Group Order", 20, 1, 1)
    Call WriteLine(strMessage, objOutputFile)

    For Each objGrp In objGrpSet
        strMessage = strPackString(objGrp.Name, 40, 1, 1)
        strMessage = strMessage + strPackString(objGrp.GroupOrder, 20, 1, 1)
        Call WriteLine(strMessage, objOutputFile)
    Next

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Sub ListJobs() - from ps.vbs
'*
'* Purpose: Gets CPU information (processes) for a machine.
'*
'* Input:   strServer           a machine name
'*          strOutputFile       an output file name
'*          strUserName         the current user's name
'*          strPassword         the current user's password
'*
'* Output:  Results are either printed on screen or saved in strOutputFile.
'*
'********************************************************************

Private Sub ListJobs(strServer       , _
                     strOutputFile   , _
                     strUserName     , _
                     strPassword     , _
                     intSortOrder    , _
                     intSortProperty , _
                     intWidth        , _
                     strProperties   , _
                     intWidths         )

    ON ERROR RESUME NEXT

    Dim objFileSystem, objOutputFile, objService, strQuery, strMessage
    Dim objEnumerator, objInstance
    Dim k, i, j, intUBound

    'Open a text file for output if the file is requested
    If Not IsEmpty(strOutputFile) Then
        If (NOT blnOpenFile(strOutputFile, objOutputFile)) Then
            Call Wscript.Echo ("Could not open an output file.")
            Exit Sub
        End If
    End If

    'Establish a connection with the server.
    If blnConnect("root\cimv2" , _
                   strUserName , _
                   strPassword , _
                   strServer   , _
                   objService  ) Then
        Call Wscript.Echo("")
        Call Wscript.Echo("Please check the server name, " _
                        & "credentials and WBEM Core.")
        Exit Sub
    End If

    'Set the query string.
    strQuery = "Select processid, name, executablepath From Win32_Process"

    'Now execute the query.

    intUBound = UBound(strProperties)
    'Need to use redim so the last dimension can be resized
    ReDim strResults(intUBound, 0), intOrder(0), strArray(0)

    Set objEnumerator = objService.ExecQuery(strQuery,,0)
    If Err.Number Then
        Print "Error 0x" & CStr(Hex(Err.Number)) & " occurred during the query."
        If Err.Description <> "" Then
            Print "Error description: " & Err.Description & "."
        End If
        Err.Clear
        Exit Sub
    End If

    'Properties to get
    strProperties(0) = "processid"
    strProperties(1) = "name"
    strProperties(2) = "executablepath"
    intWidths(1) = 15
    intWidths(0) = 15
    intWidths(2) = 40

    'Read properties of processes into arrays.
    i = 0
    For Each objInstance in objEnumerator
        If objInstance is nothing Then
            Exit For
        End If
        ReDim Preserve strResults(intUBound, i), intOrder(i), strArray(i)
        For j = 0 To intUBound
            Select Case LCase(strProperties(j)) 
                Case "processid" 
                    strResults(j, i) = objInstance.properties_(strProperties(j))
                    If strResults(j, i) < 0 Then
                        '4294967296 is 0x100000000.
                        strResults(j, i) = CStr(strResults(j, i) + 4294967296)
                    End If
                Case "owner"
                    Dim strDomain, strUser
                    Call objInstance.GetOwner(strUser, strDomain)
                    strResults(j, i) = strDomain & "\" & strUser
                Case Else
                    strResults(j, i) = CStr _
                        (objInstance.properties_(strProperties(j)))
            End Select
            If Err.Number Then
                Err.Clear
                strResults(j, i) = "(null)"
            End If
        Next
        intOrder(i) = i
        'Copy the property values to be sorted.
        strArray(i) = strResults(0, i)
        i = i + 1
        If Err.Number Then
            Err.Clear
        End If
    Next

    'Check the data type of the property to be sorted
    k = CDbl(strArray(0))
    If Err.Number Then      'not a number
        Err.Clear
    Else                    'a number
        'Pack empty spaces at the begining of each number
        For j = 0 To UBound(strArray)
            'Assume the longest number would be less than 40 digits.
            strArray(j) = strPackString(strArray(j), 40, 0, 0)
        Next
    End If

    If i > 0 Then
        'Print the header
        strMessage = vbCRLF & Space(2)
        For j = 0 To intUBound
            strMessage = strMessage & UCase(strPackString(strProperties(j), _
                intWidths(j), 1, True))
        Next
        WriteLine strMessage & vbCRLF, objOutputFile

        'Sort strArray
        Call SortArray(strArray, 1, intOrder, 0)

            For j = 0 To intUBound
                'First copy results to strArray and change the order of elements
                For k = 0 To i-1    'i is number of instances retrieved.
                    strArray(k) = strResults(j, intOrder(k))
                Next
                'Now copy results back to strResults.
                For k = 0 To i-1    'i is number of instances retrieved.
                    strResults(j, k) = strArray(k)
                Next
            Next

        For k = 0 To i-1
            strMessage = Space(2)
            For j = 0 To intUBound
                strMessage = strMessage & strPackString(strResults(j, k), _
                    intWidths(j), 1, True)
            Next
            WriteLine strMessage, objOutputFile
        Next
    End If

    If IsObject(objOutputFile) Then
        objOutputFile.Close
        Call Wscript.Echo ("Results are saved in file " & strOutputFile & ".")
    End If

End Sub

'********************************************************************
'*
'* Function intParseCmdLine()
'*
'* Purpose: Parses the command line.
'* Input:   
'*
'* Output:  strServer         a remote server ("" = local server")
'*          strUserName       the current user's name
'*          strPassword       the current user's password
'*          strOutputFile     an output file name
'*
'********************************************************************
Private Function intParseCmdLine( ByRef strServer,        _
                                  ByRef strUserName,      _
                                  ByRef strPassword,      _
                                  ByRef strOutputFile     )

    ON ERROR RESUME NEXT

    Dim strFlag
    Dim intState, intArgIter
    Dim objFileSystem

    If Wscript.Arguments.Count > 0 Then
        strFlag = Wscript.arguments.Item(0)
    End If

    If IsEmpty(strFlag) Then                'No arguments have been received
        intParseCmdLine = CONST_PROCEED
        Exit Function
    End If

    'Check if the user is asking for help or is just confused
    If (strFlag="help") OR (strFlag="/h") OR (strFlag="\h") OR (strFlag="-h") _
        OR (strFlag = "\?") OR (strFlag = "/?") OR (strFlag = "?") _ 
        OR (strFlag="h") Then
        intParseCmdLine = CONST_SHOW_USAGE
        Exit Function
    End If

    'Retrieve the command line and set appropriate variables
     intArgIter = 0
    Do While intArgIter <= Wscript.arguments.Count - 1
        Select Case Left(LCase(Wscript.arguments.Item(intArgIter)),2)
  
            Case "/s"
                If Not blnGetArg("Server", strServer, intArgIter) Then
                    intParseCmdLine = CONST_ERROR
                    Exit Function
                End If
                intArgIter = intArgIter + 1

            Case "/o"
                If Not blnGetArg("Output File", strOutputFile, intArgIter) Then
                    intParseCmdLine = CONST_ERROR
                    Exit Function
                End If
                intArgIter = intArgIter + 1

            Case "/u"
                If Not blnGetArg("User Name", strUserName, intArgIter) Then
                    intParseCmdLine = CONST_ERROR
                    Exit Function
                End If
                intArgIter = intArgIter + 1

            Case "/w"
                If Not blnGetArg("User Password", strPassword, intArgIter) Then
                    intParseCmdLine = CONST_ERROR
                    Exit Function
                End If
                intArgIter = intArgIter + 1

            Case Else 'We shouldn't get here
                Call Wscript.Echo("Invalid or misplaced parameter: " _
                   & Wscript.arguments.Item(intArgIter) & vbCRLF _
                   & "Please check the input and try again," & vbCRLF _
                   & "or invoke with '/?' for help with the syntax.")
                Wscript.Quit

        End Select

    Loop '** intArgIter <= Wscript.arguments.Count - 1

    If IsEmpty(intParseCmdLine) Then _
        intParseCmdLine = CONST_PROCEED

End Function

'********************************************************************
'*
'* Sub ShowUsage()
'*
'* Purpose: Shows the correct usage to the user.
'*
'* Input:   None
'*
'* Output:  Help messages are displayed on screen.
'*
'********************************************************************
Private Sub ShowUsage()

    Wscript.Echo ""
    Wscript.Echo "Displays information on system BIOS."
    Wscript.Echo ""
    Wscript.Echo "SYNTAX:"
    Wscript.Echo "  ViewAll.vbs [/S <server>] [/U <username>]" _
                &" [/W <password>]"
    Wscript.Echo "  [/O <outputfile>]"
    Wscript.Echo ""
    Wscript.Echo "PARAMETER SPECIFIERS:"
    Wscript.Echo "   server        A machine name."
    Wscript.Echo "   username      The current user's name."
    Wscript.Echo "   password      Password of the current user."
    Wscript.Echo "   outputfile    The output file name."
    Wscript.Echo ""
    Wscript.Echo "EXAMPLE:"
    Wscript.Echo "1. cscript ViewAll.vbs"
    Wscript.Echo "   Get the bios information for the current machine."
    Wscript.Echo "2. cscript ViewAll.vbs /S MyMachine2"
    Wscript.Echo "   Get the bios information for the machine MyMachine2."

End Sub

'********************************************************************
'* General Routines
'********************************************************************

'********************************************************************
'*
'* Function strFormatMOFTime(strDate)
'*
'* Purpose: Formats the date in WBEM to a readable Date
'*
'* Input:   blnB    A WBEM Date
'*
'* Output:  a string 
'*
'********************************************************************

Private Function strFormatMOFTime(strDate)
	Dim str
	str = Mid(strDate,1,4) & "-" _
           & Mid(strDate,5,2) & "-" _
           & Mid(strDate,7,2) & ", " _
           & Mid(strDate,9,2) & ":" _
           & Mid(strDate,11,2) & ":" _
           & Mid(strDate,13,2)
	strFormatMOFTime = str
End Function

'********************************************************************
'*
'*  Function strLimitStringLengthSlashes(strInput, intMaxLength, intIndent)
'*
'*
'*  Purpose: Limits a string so it will fit on a standard screen
'*           without wrapping.
'*
'*  Input:   strInput       The string which size will be limited.
'*           intMaxLength   The maximum length to allow a string to be.
'*           intIndent      The length of any indents on wrapping lines.
'*
'*  Output:  A string.
'*
'*
'********************************************************************

Function strLimitStringLengthSlashes(strInput, intMaxLength, intIndent)
    On Error Resume Next

    Dim strSearchCharacter
    Dim I
    Dim J
        
    If Len(strInput) > intMaxLength then
        strSearchCharacter = ""
        I = len(strInput)
        Do Until Len(Left(strInput,I)) < intMaxLength
            Do Until Mid(strInput,I,1) = "\"
                I = I - 1
                If I = 0 then
                    strLimitStringLengthSlashes = "Line is too long to display"
                    Exit Function
                End If
            Loop
            I = I - 1
        Loop
        strLimitStringLengthSlashes = Left(strInput,I) & vbCRLF
        For J = 1 to intIndent
            strLimitStringLengthSlashes = strLimitStringLengthSlashes & " "
        Next
        If Len(Right(strInput,Len(strInput) - I)) > intMaxLength  then
            strLimitStringLengthSlashes = "Line is too long to display"
            Exit Function
        Else
            strLimitStringLengthSlashes = strLimitStringLengthSlashes _
                                          & Right(strInput,Len(strInput) - I)
        End If
    Else
        strLimitStringLengthSlashes = strInput
    End If

End Function

'********************************************************************
'*
'* Function strPackString()
'*
'* Purpose: Attaches spaces to a string to increase the length to intWidth.
'*
'* Input:   strString   a string
'*          intWidth    the intended length of the string
'*          blnAfter    Should spaces be added after the string?
'*          blnTruncate specifies whether to truncate the string or not if
'*                      the string length is longer than intWidth
'*
'* Output:  strPackString is returned as the packed string.
'*
'********************************************************************
Private Function strPackString( ByVal strString, _
                                ByVal intWidth,  _
                                ByVal blnAfter,  _
                                ByVal blnTruncate)

    ON ERROR RESUME NEXT

    intWidth      = CInt(intWidth)
    blnAfter      = CBool(blnAfter)
    blnTruncate   = CBool(blnTruncate)

    If Err.Number Then
        Call Wscript.Echo ("Argument type is incorrect!")
        Err.Clear
        Wscript.Quit
    End If

    If IsNull(strString) Then
        strPackString = "null" & Space(intWidth-4)
        Exit Function
    End If

    strString = CStr(strString)
    If Err.Number Then
        Call Wscript.Echo ("Argument type is incorrect!")
        Err.Clear
        Wscript.Quit
    End If

    If intWidth > Len(strString) Then
        If blnAfter Then
            strPackString = strString & Space(intWidth-Len(strString))
        Else
            strPackString = Space(intWidth-Len(strString)) & strString & " "
        End If
    Else
        If blnTruncate Then
            strPackString = Left(strString, intWidth-1) & " "
        Else
            strPackString = strString & " "
        End If
    End If

End Function

'********************************************************************
'* 
'*  Function blnGetArg()
'*
'*  Purpose: Helper to intParseCmdLine()
'* 
'*  Usage:
'*
'*     Case "/s" 
'*       blnGetArg ("server name", strServer, intArgIter)
'*
'********************************************************************
Private Function blnGetArg ( ByVal StrVarName,   _
                             ByRef strVar,       _
                             ByRef intArgIter) 

    blnGetArg = False 'failure, changed to True upon successful completion

    If Len(Wscript.Arguments(intArgIter)) > 2 then
        If Mid(Wscript.Arguments(intArgIter),3,1) = ":" then
            If Len(Wscript.Arguments(intArgIter)) > 3 then
                strVar = Right(Wscript.Arguments(intArgIter), _
                         Len(Wscript.Arguments(intArgIter)) - 3)
                blnGetArg = True
                Exit Function
            Else
                intArgIter = intArgIter + 1
                If intArgIter > (Wscript.Arguments.Count - 1) Then
                    Call Wscript.Echo( "Invalid " & StrVarName & ".")
                    Call Wscript.Echo( "Please check the input and try again.")
                    Exit Function
                End If

                strVar = Wscript.Arguments.Item(intArgIter)
                If Err.Number Then
                    Call Wscript.Echo( "Invalid " & StrVarName & ".")
                    Call Wscript.Echo( "Please check the input and try again.")
                    Exit Function
                End If

                If InStr(strVar, "/") Then
                    Call Wscript.Echo( "Invalid " & StrVarName)
                    Call Wscript.Echo( "Please check the input and try again.")
                    Exit Function
                End If

                blnGetArg = True 'success
            End If
        Else
            strVar = Right(Wscript.Arguments(intArgIter), _
                     Len(Wscript.Arguments(intArgIter)) - 2)
            blnGetArg = True 'success
            Exit Function
        End If
    Else
        intArgIter = intArgIter + 1
        If intArgIter > (Wscript.Arguments.Count - 1) Then
            Call Wscript.Echo( "Invalid " & StrVarName & ".")
            Call Wscript.Echo( "Please check the input and try again.")
            Exit Function
        End If

        strVar = Wscript.Arguments.Item(intArgIter)
        If Err.Number Then
            Call Wscript.Echo( "Invalid " & StrVarName & ".")
            Call Wscript.Echo( "Please check the input and try again.")
            Exit Function
        End If

        If InStr(strVar, "/") Then
            Call Wscript.Echo( "Invalid " & StrVarName)
            Call Wscript.Echo( "Please check the input and try again.")
            Exit Function
        End If
        blnGetArg = True 'success
    End If
End Function

'********************************************************************
'*
'* Function blnConnect()
'*
'* Purpose: Connects to machine strServer.
'*
'* Input:   strServer       a machine name
'*          strNameSpace    a namespace
'*          strUserName     name of the current user
'*          strPassword     password of the current user
'*
'* Output:  objService is returned  as a service object.
'*          strServer is set to local host if left unspecified
'*
'********************************************************************
Private Function blnConnect(ByVal strNameSpace, _
                            ByVal strUserName,  _
                            ByVal strPassword,  _
                            ByRef strServer,    _
                            ByRef objService)

    ON ERROR RESUME NEXT

    Dim objLocator, objWshNet

    blnConnect = False     'There is no error.

    'Create Locator object to connect to remote CIM object manager
    Set objLocator = CreateObject("WbemScripting.SWbemLocator")
    If Err.Number then
        Call Wscript.Echo( "Error 0x" & CStr(Hex(Err.Number)) & _
                           " occurred in creating a locator object." )
        If Err.Description <> "" Then
            Call Wscript.Echo( "Error description: " & Err.Description & "." )
        End If
        Err.Clear
        blnConnect = True     'An error occurred
        Exit Function
    End If

    'Connect to the namespace which is either local or remote
    Set objService = objLocator.ConnectServer (strServer, strNameSpace, _
       strUserName, strPassword)
    ObjService.Security_.impersonationlevel = 3
    If Err.Number then
        Call Wscript.Echo( "Error 0x" & CStr(Hex(Err.Number)) & _
                           " occurred in connecting to server " _
           & strServer & ".")
        If Err.Description <> "" Then
            Call Wscript.Echo( "Error description: " & Err.Description & "." )
        End If
        Err.Clear
        blnConnect = True     'An error occurred
    End If

    'Get the current server's name if left unspecified
    If IsEmpty(strServer) Then
        Set objWshNet = CreateObject("Wscript.Network")
    strServer     = objWshNet.ComputerName
    End If

End Function

'********************************************************************
'*
'* Sub      VerifyHostIsCscript()
'*
'* Purpose: Determines which program is used to run this script.
'*
'* Input:   None
'*
'* Output:  If host is not cscript, then an error message is printed 
'*          and the script is aborted.
'*
'********************************************************************
Sub VerifyHostIsCscript()

    ON ERROR RESUME NEXT

    Dim strFullName, strCommand, i, j, intStatus

    strFullName = WScript.FullName

    If Err.Number then
        Call Wscript.Echo( "Error 0x" & CStr(Hex(Err.Number)) & " occurred." )
        If Err.Description <> "" Then
            Call Wscript.Echo( "Error description: " & Err.Description & "." )
        End If
        intStatus =  CONST_ERROR
    End If

    i = InStr(1, strFullName, ".exe", 1)
    If i = 0 Then
        intStatus =  CONST_ERROR
    Else
        j = InStrRev(strFullName, "\", i, 1)
        If j = 0 Then
            intStatus =  CONST_ERROR
        Else
            strCommand = Mid(strFullName, j+1, i-j-1)
            Select Case LCase(strCommand)
                Case "cscript"
                    intStatus = CONST_CSCRIPT
                Case "wscript"
                    intStatus = CONST_WSCRIPT
                Case Else       'should never happen
                    Call Wscript.Echo( "An unexpected program was used to " _
                                       & "run this script." )
                    Call Wscript.Echo( "Only CScript.Exe or WScript.Exe can " _
                                       & "be used to run this script." )
                    intStatus = CONST_ERROR
                End Select
        End If
    End If

    If intStatus <> CONST_CSCRIPT Then
        Call WScript.Echo( "Please run this script using CScript." & vbCRLF & _
             "This can be achieved by" & vbCRLF & _
             "1. Using ""CScript ViewAll.vbs arguments"" for Windows 95/98 or" _
             & vbCRLF & "2. Changing the default Windows Scripting Host " _
             & "setting to CScript" & vbCRLF & "    using ""CScript " _
             & "//H:CScript //S"" and running the script using" & vbCRLF & _
             "    ""ViewAll.vbs arguments"" for Windows NT/2000." )
        WScript.Quit
    End If

End Sub

'********************************************************************
'*
'* Sub WriteLine()
'* Purpose: Writes a text line either to a file or on screen.
'* Input:   strMessage  the string to print
'*          objFile     an output file object
'* Output:  strMessage is either displayed on screen or written to a file.
'*
'********************************************************************
Sub WriteLine(ByVal strMessage, ByVal objFile)

    On Error Resume Next
    If IsObject(objFile) then        'objFile should be a file object
        objFile.WriteLine strMessage
    Else
        Call Wscript.Echo( strMessage )
    End If

End Sub

'********************************************************************
'* 
'* Function blnErrorOccurred()
'*
'* Purpose: Reports error with a string saying what the error occurred in.
'*
'* Input:   strIn		string saying what the error occurred in.
'*
'* Output:  displayed on screen 
'* 
'********************************************************************
Private Function blnErrorOccurred (ByVal strIn)

    If Err.Number Then
        Call Wscript.Echo( "Error 0x" & CStr(Hex(Err.Number)) & ": " & strIn)
        If Err.Description <> "" Then
            Call Wscript.Echo( "Error description: " & Err.Description)
        End If
        Err.Clear
        blnErrorOccurred = True
    Else
        blnErrorOccurred = False
    End If

End Function

'********************************************************************
'* 
'* Function blnOpenFile
'*
'* Purpose: Opens a file.
'*
'* Input:   strFileName		A string with the name of the file.
'*
'* Output:  Sets objOpenFile to a FileSystemObject and setis it to 
'*            Nothing upon Failure.
'* 
'********************************************************************
Private Function blnOpenFile(ByVal strFileName, ByRef objOpenFile)

    ON ERROR RESUME NEXT

    Dim objFileSystem

    Set objFileSystem = Nothing

    If IsEmpty(strFileName) OR strFileName = "" Then
        blnOpenFile = False
        Set objOpenFile = Nothing
        Exit Function
    End If

    'Create a file object
    Set objFileSystem = CreateObject("Scripting.FileSystemObject")
    If blnErrorOccurred("Could not create filesystem object.") Then
        blnOpenFile = False
        Set objOpenFile = Nothing
        Exit Function
    End If

    'Open the file for output
    Set objOpenFile = objFileSystem.OpenTextFile(strFileName, 8, True)
    If blnErrorOccurred("Could not open") Then
        blnOpenFile = False
        Set objOpenFile = Nothing
        Exit Function
    End If
    blnOpenFile = True

End Function

'********************************************************************
'*                                                                  *
'*                           End of File                            *
'*                                                                  *
'********************************************************************
