# 소켓 테스트

소켓 DLL 을 테스트하기 위한 프로젝트.  
델파이 7에서 빌드를 기준으로 한다.  

DLL이 사용하는 통신 구조는 다음과 같다. 
ScramblePacket 빌드이므로 패킷은 해싱 및 암호화된다.  

```pas
  TPacketRec = packed record
    Len : U16; // PID+Data의 길이를 합친 값.
    PID : U8;  // 패킷 아이디.
    case Integer of
      0: (Data: TU8Array);
{$IFDEF ScramblePacket}
      1: (HashVal: U32; Number: U8; Data2: TU8Array); // 해싱+넘버링 할 때 정보.
      2: (HashArr: Array[0..3] of Byte);
{$ENDIF}
  end;
```

DLL 사용자 입장에서는 패킷아이디(PID)와 디코딩된 패킷 데이터가 제공된다. 


## 서버 DLL 사용법. 

DLL 이름 : SSockE.dll



### 참고사항 

* 접속 후 3초이내에 최초 패킷이 없으면 접속 종료.  
* 접속 중 30초 이내에 아무 패킷이 없으면 접속 종료.  



## 클라 DLL 사용법. 

DLL 이름 : CSockE.dll

