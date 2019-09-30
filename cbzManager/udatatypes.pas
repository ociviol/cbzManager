unit uDataTypes;

{
 Ollivier Civiol - 2019
 ollivier@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  PInteger = ^Integer;
  TImgOperation = (opNone, opFlipH, opFlipV, opRotateL, opRotateR,
                   opCropBorders, opSplit, opConvert, opDeleteFile);
  TImgOperations = set of TImgOperation;
  TDataType = (dtImage, dtMeta, dtPdf);

  TArcType = (arcZip, arc7Zip, arcRar, arcPdf, arcUnknown);
  TIncProgressEvent = procedure(const ProgressID : integer; const Msg : String = '') of object;
  TCbzFoundFileCallback = procedure(const Filename : String) of object;
  TCbzProgressEvent = procedure(Sender : TObject; const ProgressID : QWord; const Pos, Max : integer; const Msg : String = '') of object;
  TStampReadyEvent = procedure(ProgressID : QWord; Index : integer) of object;
  TOperation = (opAdd, opDelete, opReplace);


implementation


end.

