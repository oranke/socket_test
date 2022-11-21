{-----------------------------------------------------------------------------
 Unit Name: zhTypes
 Author:    oranke_f
 Date:      2010-3-10
 Purpose:   벡터, 칼라 등 엔진 전체에서 사용되는 타입의 정의.
 History:

  2010-03-10
    예전 유니트를 재정리중.
    U32, I32 를 U2, I2 로 재정의할 것 고민중임.

  2010-04-28
    SmallPoint, SmallRect 를 I16Point, I16Rect 로 변경.

-----------------------------------------------------------------------------}



{$ifdef FPC}
  {$mode delphi}
{$endif}

{.$I directives.inc}

unit zhTypes;

interface

uses
  Windows;


type
  // 형 구분을 명확히 하기 위해 기본형을 재정의.
  PI8         = ^I8;
  PI16        = ^I16;
  PI32        = ^I32;

  PU8         = ^U8;
  PU16        = ^U16;
  PU32        = ^U32;

  PI64        = ^I64;

  PF32        = ^F32;
  PF64        = ^F64;
  PF80        = ^F80;

  //PP32        = ^P32;

  I8          = ShortInt;
  I16         = SmallInt;
  I32         = LongInt;

  U8          = Byte;
  U16         = Word;
  U32         = LongWord;

  I64         = Int64;

  F32         = Single;
  F64         = Double;
  F80         = Extended;

  //P32         = Pointer;

  //xFixed          = I32;      // 정수부 16비트, 실수부 16비트의 고정소수점. Ogl-es 의 GLFixed와 동일.

type
  PVector2sw   = ^TVector2sw;
  TVector2sw   = array [0..1] of I16;  // 패킹된 텍스쳐 좌표에 사용.

  PVector2f    = ^TVector2f;
  TVector2f    = array [0..1] of F32;

  PVector2sb   = ^TVector2sb;
  TVector2sb   = array [0..1] of I8;   // 패킹된 2차원 노멀값 등에 사용. 

  PVector2b    = ^TVector2b;
  TVector2b    = array [0..1] of U8;

  PVector3sb   = ^TVector3sb;
  TVector3sb   = array [0..2] of I8;   // 패킹된 노멀값 등에 사용.

  PVector3b    = ^TVector3b;
  TVector3b    = Array [0..2] of U8;   // 노멀맵의 값 참조에 사용.


  PVector3sw  = ^TVector3sw;
  TVector3sw  = array [0..2] of I16;

  PVector3i   = ^TVector3i;
  TVector3i   = array [0..2] of I32;

  PVector3f   = ^TVector3f;
  TVector3f   = array [0..2] of F32;

  PVector3d   = ^TVector3d;
  TVector3d   = array [0..2] of F64;

  PVector4sw  = ^TVector4sw;
  TVector4sw  = array [0..3] of I16;  // 패킹된 쿼터니온 등에 사용.

  PVector4i   = ^TVector4i;
  TVector4i   = array [0..3] of I32;

  PVector4f   = ^TVector4f;
  TVector4f   = array [0..3] of F32;

  PVector4d   = ^TVector4d;
  TVector4d   = array [0..3] of F64;

  PVector4p   = ^TVector4p;
  TVector4p   = array[0..3] of Pointer;

  PMatrix3f   = ^TMatrix3f;
  TMatrix3f   = array [0..2] of TVector3f;

  PMatrix4f   = ^TMatrix4f;
  TMatrix4f   = array [0..3] of TVector4f;

  PMatrix4d   = ^TMatrix4d;
  TMatrix4d   = array [0..3] of TVector4d;
  

  PPlane      = ^TPlane;
  TPlane = packed record // ax+by+cz+d. a,b,c,d 저장.
    case Integer of
      0 : (x, y, z, d: F32);
      1 : (Vector3f: TVector3f; vW: F32);
      2 : (Vector4f: TVector4f);
  end;

  {
  PZhVector = ^TZhVector;
  TZhVector = packed record
    case Integer of
      0: (x : F32;
          y : F32;
          z : F32);
      1: (v : TVector3f);
  end;
  }

  PAxisAngle = ^TAxisAngle;
  TAxisAngle = packed record
    case Integer of
      0: (Axis : TVector3f;
          Angle: F32);
      1: (Vector : TVector4f);
  end;


  // q = ([x, y, z], w)
  PQuaternion = ^TQuaternion;
  TQuaternion = packed record
    case Integer of
      0: (ImagPart: TVector3f;
          RealPart: F32);
      1: (X, Y, Z, W: F32);
      2: (Vector: TVector4f);
  end;

  // 회전 쿼터니온의 각 성분을 SmallInt로 패킹한 것..
  PPackQuaternion = ^TPackQuaternion;
  TPackQuaternion = packed record
    case Integer of
      0: (ImagPart: Array[0..2] of I16;
          RealPart: I16);
      1: (X, Y, Z, W: I16);
      2: (Vector: TVector4sw);
  end;

  PAABBBox = ^TAABBBox;
  TAABBBox = record // 축 경계상자.
    Min, Max: TVector3f;
  end;

  PSphere = ^TSphere;
  TSphere = packed record // 경계구.
    case Integer of
      0: (Center: TVector3f;
          Rad : F32);
      1: (Vector: TVector4f);
  end;

  PFrustum = ^TFrustum;
  TFrustum = packed record // 프러스텀.
    Case Integer of
      0: (m : Array[0..5] of TPlane);
      1: (R, L, B, T, F, N: TPlane);
  end;


  TZhVector4f = packed record
    case Integer of
      0: (x, y, z, w: Single);
      1: (Vector4: TVector4f);
      2: (Quaternion: TQuaternion);
      3: (Sphere: TSphere);
      4: (Vector3: TVector3f; vW: Single);
  end;



{
  * TZhMatrix
  // D3D및 OGL, 기타 루틴들과 호환되도록 공용체로 선언.
  // 값이 같은 것을 순서대로 나열하면 다음과 같다.

  ZhMatrix.m[0, 0], ZhMatrix.m[0, 1], ZhMatrix.m[0, 2], ZhMatrix.m[0, 3],
  ZhMatrix.m[1, 0], ZhMatrix.m[1, 1], ZhMatrix.m[1, 2], ZhMatrix.m[1, 3],
  ZhMatrix.m[2, 0], ZhMatrix.m[2, 1], ZhMatrix.m[2, 2], ZhMatrix.m[2, 3],
  ZhMatrix.m[3, 0], ZhMatrix.m[3, 1], ZhMatrix.m[3, 2], ZhMatrix.m[3, 3]

  ZhMatrix._11, ZhMatrix._12, ZhMatrix._13, ZhMatrix._14,
  ZhMatrix._21, ZhMatrix._22, ZhMatrix._23, ZhMatrix._24,
  ZhMatrix._31, ZhMatrix._32, ZhMatrix._33, ZhMatrix._34,
  ZhMatrix._41, ZhMatrix._42, ZhMatrix._43, ZhMatrix._44

  ZhMatrix.mAR[0], ZhMatrix.mAR[1], ZhMatrix.mAR[2], ZhMatrix.mAR[3],
  ZhMatrix.mAR[4], ZhMatrix.mAR[5], ZhMatrix.mAR[6], ZhMatrix.mAR[7],
  ZhMatrix.mAR[8], ZhMatrix.mAR[9], ZhMatrix.mAR[10], ZhMatrix.mAR[11],
  ZhMatrix.mAR[12], ZhMatrix.mAR[13], ZhMatrix.mAR[14], ZhMatrix.mAR[15]

}

  PZhMatrix = ^TZhMatrix;
  TZhMatrix = packed record // 메트릭스의 각 성분을 여러가지 방법으로 접근하기 위한 공용체
  case Integer of
    0: (_11, _12, _13, _14: F32;
        _21, _22, _23, _24: F32;
        _31, _32, _33, _34: F32;
        _41, _42, _43, _44: F32);
    1: (mAR : Array[0..15] of F32);
    2: (m : TMatrix4f);
    3: (vX : TVector3f; v1: F32; // 벡터 성분과 실수부로 뽑아낼 때 사용..
        vY : TVector3f; v2: F32;
        vZ : TVector3f; v3: F32;
        vT : TVector3f; v4: F32);
  end;
  
  // 64비트 변수 처리용 구조체.
  PLongLong = ^TLongLong;
  TLongLong = packed record
  case Integer of
    0: (SInt1, SInt2, SInt3, SInt4: I16);
    1: (W1, W2, W3, W4: U16);
    2: (Int1, Int2: I32);
    3: (LID1, LID2: U32);
    4: (LLID: I64);
  end;

  PU8Array  = ^TU8Array;
  TU8Array  = array[0..MAXINT shr 1] of U8;


  PVector2swArray = ^TVector2swArray;
  TVector2swArray = array [0..MAXINT shr 3] of TVector2sw;

  PVector2fArray  = ^TVector2fArray;
  TVector2fArray  = array [0..MAXINT shr 4] of TVector2f;

  PVector3sbArray = ^TVector3sbArray;
  TVector3sbArray = array [0..MAXINT shr 2] of TVector3sb;

  //PVector3bArray  = ^TVector3bArray;
  //TVector3bArray  = array [0..MAXINT shr 2] of TVector3b;

  PVector3fArray  = ^TVector3fArray;
  TVector3fArray  = array [0..MAXINT shr 4] of TVector3f;

  PVector4fArray  = ^TVector4fArray;
  TVector4fArray  = array [0..MAXINT shr 5] of TVector4f;

  PI16Array = ^TI16Array;
  TI16Array = array[0..MAXINT shr 2] of I16;

  PU16Array = ^TU16Array;
  TU16Array = array[0..MAXINT shr 2] of U16;

  PI32Array  = ^TI32Array;
  TI32Array  = array[0..MAXINT shr 3] of I32;

  PU32Array = ^TU32Array;
  TU32Array = array[0..MAXINT shr 3] of U32;

  PI64Array = ^TI64Array;
  TI64Array = array[0..MAXINT shr 4] of I64;

  PLongLongArray = ^TLongLongArray;
  TLongLongArray = array[0..MAXINT shr 4] of TLongLong;

  PF32Array = ^TF32Array;
  TF32Array = array [0..MaxInt shr 3] of F32;

  PPointerArray = ^TPointerArray;
  TPointerArray = array [0..MAXINT shr 3] of Pointer;


  // 위치/영역값 조작을 위한 정의. 게임 내에서 위치 지정에
  // 32비트 정수를 사용할 이유가 없으므로 줄여 쓰도록 한다.
  PI16Point = ^TI16Point;
  TI16Point = packed record
    case Integer of
      0: (X, Y : I16); // Low : X, High : Y.
      1: (LID : U32);
  end;

  PI16Size = ^TI16Size;
  TI16Size = TI16Point;

  PI16Rect = ^TI16Rect;
  TI16Rect = packed record
    case Integer of
      0 : (Left, Top, Right, Bottom: I16);
      1 : (TopLeft, BottomRight: TSmallPoint);
      2 : (LLID : I64);
  end;


  // 실수형 위치값. 8바이트.
  PF32Point = ^TF32Point; 
  TF32Point = packed record
    case Integer of
      0: (X, Y: F32);
      1: (Vt: TVector2f);
      2: (LID1, LID2: U32);
      3: (LLID: I64);
  end;

  PF32Size = ^TF32Size;
  TF32Size = TF32Point;

  PF32Rect = ^TF32Rect;
  TF32Rect = packed record
    case Integer of
      0: (Left, Top, Right, Bottom: F32);
      1: (TopLeft, BottomRight: TF32Point);
  end;

  
  PTick = ^TTick;
  TTick = {$IFDEF USE_U16_TICK_VECTOR}U16{$ELSE}U32{$ENDIF};

  
  // IP를 담아두기 위한 32비트 데이터. 
  PIPData = ^TIPData;
  TIPData = packed record
  case Integer of
    0: (IP1, IP2, IP3, IP4: U8);
    1: (LID: U32);
  end;

  // UTF8 문자열 재정의. 
  UTF8Char = AnsiChar;
  PUTF8Char = PAnsiChar;
  
  UTF8ShortString = ShortString;
  PUTF8ShortString = PShortString;
  

  
implementation



initialization

finalization

end.
