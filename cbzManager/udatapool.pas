unit uDataPool;

{$mode objfpc}{$H+}

interface

uses
  Classes, uDataItem, Utils.Logger,
{$if defined(Darwin) or defined(Linux)}
  cthreads,
{$endif}
  contnrs;

//{$define MONO_THREAD}

type
  TThreadDataPool = Class
  private
    FLog : ILog;
    FWebpQualityFactor : PInteger;
    FPoolSync : TThreadList;
    FPool : TObjectList;
    FThreads : TList;
    function GetPool(Index: Integer): TThreadDataItem;
    function GetPoolSize: Integer;
    function GetNbWorkers: Integer;
    procedure SetThreads(nbThreads : Integer);
  public
    constructor Create(aPoolSize : Integer; Log : ILog; aNbThreads : Integer; aWebpQualityFactor : PInteger);
    Destructor Destroy; override;
    procedure SetPerfs(aNbThreads : Integer);
    procedure Stop;
    function AddPool(aLog : ILog):Integer;
    procedure RemovePool(Pool : Integer);
    property PoolSize:Integer read GetPoolSize;
    property Pool[Index:Integer]:TThreadDataItem read GetPool;
    property NbWorkers : Integer read GetNbWorkers;
  End;

implementation

uses
  uThreadConvert;

{ TThreadDataPool }

constructor TThreadDataPool.Create(aPoolSize : Integer; Log: ILog; aNbThreads : Integer; aWebpQualityFactor : PInteger);
var
  i : integer;
begin
  inherited Create;
  FThreads := TList.Create;
  FPoolSync := TThreadList.Create;
  FWebpQualityFactor := aWebpQualityFactor;
  FLog := Log;
  FPool := TObjectList.Create;
  for i:=0 to {$ifdef MONO_THREAD} 1 {$else} aPoolSize - 1 {$endif} do
    FPool.Add(TThreadDataItem.Create(Log));

  SetPerfs(aNbThreads);
end;

destructor TThreadDataPool.Destroy;
var
  i : integer;
begin
  Stop;
  for i := 0 to FThreads.Count - 1 do
  begin
    TThread(FThreads[i]).WaitFor;
    TThread(FThreads[i]).Free;
  end;

  FPool.Free;
  FPoolSync.Free;
  FThreads.Free;
  inherited;
end;

function TThreadDataPool.AddPool(aLog : ILog):Integer;
begin
  FPoolSync.LockList;
  try
    result := FPool.Add(TThreadDataItem.Create(aLog));
  finally
    FPoolSync.UnLockList;
  end;
end;

procedure TThreadDataPool.RemovePool(Pool : Integer);
begin
  FPoolSync.LockList;
  try
    if Pool <= FPool.Count then
    begin
      TThreadDataItem(FPool[Pool-1]).LockList;
      FPool.Delete(Pool - 1);
    end;
  finally
    FPoolSync.UnLockList;
  end;
end;

procedure TThreadDataPool.SetPerfs(aNbThreads : Integer);
begin
{$ifdef MONO_THREAD}
  SetThreads(1);
{$ELSE}
  SetThreads(aNbThreads);
{$endif}
end;

procedure TThreadDataPool.SetThreads(nbThreads : Integer);
begin
  if nbThreads < 1 then
    nbThreads := 2;
  // decrease
  while FThreads.Count > nbThreads do
  begin
    TThread(FThreads[FThreads.Count-1]).Terminate;
    TThread(FThreads[FThreads.Count-1]).WaitFor;
    TThread(FThreads[FThreads.Count-1]).Free;
    FThreads.Delete(FThreads.Count-1);
  end;
  // increase
  while FThreads.Count < nbThreads do
  begin
    FThreads.Add(TThreadConvert.Create(Self, FLog, FWebpQualityFactor));
  end;
end;

procedure TThreadDataPool.Stop;
var
  i : integer;
begin
  for i := 0 to FThreads.Count - 1 do
    if not TThreadConvert(FThreads[i]).IsTerminated then
      TThread(FThreads[i]).Terminate;
end;

function TThreadDataPool.GetNbWorkers: Integer;
begin
  result := FThreads.Count;
end;

function TThreadDataPool.GetPool(Index: Integer): TThreadDataItem;
begin
  FPoolSync.LockList;
  try
    if Index < FPool.Count then
      result := TThreadDataItem(FPool[Index])
    else
      Result := nil;
  finally
    FPoolSync.UnLockList;
  end;
end;

function TThreadDataPool.GetPoolSize: Integer;
begin
  FPoolSync.LockList;
  try
    result := FPool.Count;
  finally
    FPoolSync.UnLockList;
  end;
end;


end.
