using ColdClearNet;

namespace Quader.Bot.Api.Bots;

public class ColdClear : IBot, IDisposable
{
    public BotVersion Version => BotVersion.ColdClear;
    public BotRunMode RunMode { get; private set; }

    private ColdClearNet.ColdClear? _coldClear;
    private bool _isStarted;

    public void Start(IEnumerable<Piece> queue, BotRunMode runMode = BotRunMode.Sync, Options? options = null, Weights? weights = null)
    {
        if (_isStarted)
            throw new Exception("Bot is already started");

        if (runMode == BotRunMode.Async)
            throw new Exception("Unsupported");

        RunMode = runMode;

        Options opt;
        Weights wgt;
        
        if (options == null)
        {
            opt = ColdClearNet.ColdClear.DefaultOptions;
            opt.UseHold = true;
            opt.Speculate = true;
            opt.SpawnRule = SpawnRule.Row21AndFall;
            opt.MaxNodes = (uint)Math.Pow(2, 20);
        }
        else
        {
            opt = options.Value;
        }

        wgt = weights ?? ColdClearNet.ColdClear.DefaultWeights;

        _coldClear = new ColdClearNet.ColdClear(opt, wgt, null, queue);

        _isStarted = true;
    }

    // ReSharper disable once InconsistentNaming
    public void Reset(bool[] board, int combo, bool b2b)
    {
        if (!_isStarted || _coldClear == null)
            throw new Exception("Bot is not started");

        _coldClear.Reset(board, combo, b2b);
    }

    public void Destroy()
    {
        Dispose();
    }

    public BotMove DoMove(int incomingGarbage)
    {
        if (!_isStarted || _coldClear == null)
            throw new Exception("Bot is not started");

        _coldClear.RequestNextMove(incomingGarbage);

        // TODO: Add async version
        var move = _coldClear.BlockNextMove();

        switch (move.PollStatus)
        {
            case BotPollStatus.MoveProvided:
                var m = new BotMove(BotStatus.MoveProvided)
                {
                    IsHoldUsed = move.Move.Hold,
                    Movements = move.Move.Movements,
                    MovementCount = move.Move.MovementCount
                };
                return m;
            case BotPollStatus.Waiting:
                return new BotMove(BotStatus.Waiting);
            default:
                return new BotMove(BotStatus.Dead);
        }
    }

    public void PushPiece(Piece piece)
    {
        if (!_isStarted || _coldClear == null)
            throw new Exception("Bot is not started");

        _coldClear.AddNextPieceAsync(piece);
    }

    public void Dispose()
    {
        _coldClear?.Dispose();
        _coldClear = null;
    }
}