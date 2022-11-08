using System;
using System.Diagnostics;
using System.Threading;
using Quader.Debugging.Logging;
using Nez.Persistence;
using Quader.Bot.Api;
using ZetaIpc.Runtime.Client;
using ColdClearNet;
using Quader.Bot.Api.Requests;

namespace Quader.Managers;

public class BotIpcManager : IDisposable
{
    private ILogger _logger = LoggerFactory.GetLogger<BotIpcManager>();

    private Process _ipcProcess;
    private IpcClient _ipcClient;

    private bool _isStopped;

    public BotIpcManager()
    {
        _ipcProcess = new Process();
        
        _ipcProcess.StartInfo.FileName = "Quader.Bot.Client"; // + ".exe"; // for windows
        //_ipcProcess.StartInfo.Arguments = "1234";
        //_ipcProcess.StartInfo.RedirectStandardInput = true;
        _ipcProcess.StartInfo.UseShellExecute = false;
        _ipcProcess.Start();

        _ipcClient = new IpcClient();
        _ipcClient.Initialize(10077);
        
        Thread.Sleep(1000);

        var syncRequest = Json.ToJson(new IpcRequest
        {
            Command = "sync"
        });
        var syncResponseStr = _ipcClient.Send(syncRequest);
        var syncResponse = Json.FromJson<IpcResponse>(syncResponseStr);

        _logger.Debug($"GOT RESPONSE: " + syncResponse.Message);
    }

    public void Stop()
    {
        _ipcClient.Send("stop");

        _ipcProcess.WaitForExit();
        _ipcProcess.Close();
        _ipcProcess.Dispose();

        _isStopped = true;
    }

    public void Start(Piece[] queue)
    {
        var cmd = new IpcRequest
        {
            Command = "start",
            Data = Json.ToJson(new StartRequestData()
            {
                RunMode = BotRunMode.Sync,
                Options = null,
                Queue = queue,
                Weights = null,
            })
        };

        var resp = _ipcClient.Send(Json.ToJson(cmd));

        _logger.Trace("[START]: " + resp);
    }

    public void Reset(bool[] board, int combo, bool b2b)
    {
        var cmd = new IpcRequest
        {
            Command = "reset",
            Data = Json.ToJson(new ResetRequestData()
            {
                Board = board,
                Back2Back = b2b,
                Combo = combo
            })
        };

        var resp = _ipcClient.Send(Json.ToJson(cmd));

        _logger.Trace("[RESET]: " + resp);
    }

    public BotMovementRequestData DoMove(int incomingGarbage)
    {
        var cmd = new IpcRequest
        {
            Command = "move",
            Data = Json.ToJson(new MoveRequestData()
            {
                IncomingGarbage = incomingGarbage
            })
        };

        var resp = _ipcClient.Send(Json.ToJson(cmd));

        _logger.Trace("[DoMove]: " + resp);

        var respObj = Json.FromJson<IpcResponse>(resp); //JsonConvert.DeserializeObject<IpcResponse>(resp);
        var r = Json.FromJson<BotMovementRequestData>(respObj.Data); //JsonConvert.DeserializeObject<BotMovementRequestData>(respObj.Data);

        return r;
    }

    public void PushPiece(Piece piece)
    {
        var cmd = new IpcRequest
        {
            Command = "push",
            Data = Json.ToJson(new PushPieceRequestData()
            {
                Piece = piece
            })
        };

        var resp = _ipcClient.Send(Json.ToJson(cmd));

        _logger.Trace("[PushPiece]: " + resp);
    }

    public void Dispose()
    {
        if (!_isStopped)
            Stop();
    }

    private void CheckResp(string resp)
    {
        //Insist.IsTrue();
    }
}