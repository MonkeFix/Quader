using System.Linq;
using System.Runtime.InteropServices;

namespace ColdClearNet;

public struct BotMove
{
    public Move Move { get; set; }
    public BotPollStatus PollStatus { get; set; }
    public IEnumerable<PlanPlacement> PlanPlacement { get; set; }
}

public sealed class ColdClear : IDisposable
{
    private IntPtr _bot;
    private static Options? _defaultOptions;
    private static Weights? _defaultWeights;
    private static Weights? _fastWeights;

    private readonly Book? _book;

    /// <summary>
    /// Gets the default options
    /// </summary>
    public static Options DefaultOptions
    {
        get
        {
            if (_defaultOptions == null)
            {
                ColdClearInterop.DefaultOptions(out var o);

                _defaultOptions = o;
            }

            return _defaultOptions.Value;
        }
    }

    /// <summary>
    /// Gets the default weights
    /// </summary>
    public static Weights DefaultWeights
    {
        get
        {
            if (_defaultWeights == null)
            {
                ColdClearInterop.DefaultWeights(out var w);

                _defaultWeights = w;
            }

            return _defaultWeights.Value;
        }
    }

    /// <summary>
    /// Gets the fast game config weights
    /// </summary>
    public static Weights FastWeights
    {
        get
        {
            if (_fastWeights == null)
            {
                ColdClearInterop.FastWeights(out var w);
                _fastWeights = w;
            }

            return _fastWeights.Value;
        }
    }

    /// <summary>
    /// Launches a bot thread with a blank board, all seven pieces in the bag, and empty queue
    /// using default options and weights.
    /// </summary>
    public ColdClear() 
        : this(DefaultOptions, DefaultWeights) { }

    public ColdClear(Options options, Weights weights)
        : this(options, weights, book: null) { }

    public ColdClear(Options options, Weights weights, Book? book)
        : this(options, weights, book, null) { }

    public ColdClear(Options options, Weights weights, IEnumerable<Piece>? queue)
        : this(options, weights, null, queue) { }

    public ColdClear(Options options, Weights weights, Book? book, IEnumerable<Piece>? queue)
    {
        var queueArr = queue?.ToArray();

        if (book != null)
            _book = book;

        _bot = ColdClearInterop.LaunchAsync(
            options,
            weights,
            book?._book ?? IntPtr.Zero,
            queueArr == null ? Array.Empty<Piece>() : queueArr,
            queueArr == null ? 0U : (uint)queueArr.Length
        );
    }

    /// <summary>
    /// Launches a bot thread with a predefined field, empty queue, remaining pieces in the bag, hold
    /// piece, back-to-back status, and combo count. This allows you to start CC from the middle of a
    /// game.
    /// </summary>
    /// <param name="options"></param>
    /// <param name="weights"></param>
    /// <param name="book"></param>
    /// <param name="field">An array of 400 booleans in row major order, with index 0 being the bottom-left cell.</param>
    /// <param name="bagRemain"></param>
    /// <param name="hold">Current hold piece</param>
    /// <param name="backToBack"></param>
    /// <param name="combo"></param>
    /// <param name="queue"></param>
    public ColdClear(
        Options options, 
        Weights weights, 
        Book? book, 
        bool[] field,
        int bagRemain,
        ref Piece hold, 
        bool backToBack = false, 
        int combo = 0, 
        IEnumerable<Piece>? queue = null
        )
    {
        var queueArr = queue?.ToArray();

        if (book != null)
            _book = book;

        _bot = ColdClearInterop.LaunchWithBoardAsync(
            options,
            weights,
            book?._book ?? IntPtr.Zero,
            field.Select(b => b ? (byte)1 : (byte)0).ToArray(),
            (uint)bagRemain, 
            ref hold, 
            backToBack, 
            (uint)combo,
            queueArr == null ? Array.Empty<Piece>() : queueArr,
            queueArr == null ? 0U : (uint)queueArr.Length
        );
    }

    /// <summary>
    /// Adds a new piece to the end of the queue.
    /// If speculation is enabled, the piece must be in the bag. For example, if you start a new
    /// game with starting sequence IJOZT, the first time you call this function you can only
    /// provide either an L or an S piece.
    /// </summary>
    /// <param name="piece"></param>
    /// <returns></returns>
    public void AddNextPieceAsync(Piece piece)
    {
        ColdClearInterop.AddNextPieceAsync(_bot, piece);
        //return Task.CompletedTask;
    }

    public void RequestNextMove(int incomingGarbage)
    {
        ColdClearInterop.RequestNextMove(_bot, (uint) incomingGarbage);
    }
    
    public BotPollStatus PollNextMove(out Move move, PlanPlacement[] plan, ref uint planLength)
    {
        return ColdClearInterop.PollNextMove(_bot, out move, plan, ref planLength);
    }

    public BotMove PollNextMove(int planLength = 0)
    {
        Move move = new Move();
        var pl = (uint)planLength;
        var pm = new PlanPlacement[planLength == 0 ? 1 : planLength];

        var status = ColdClearInterop.PollNextMove(_bot, out move, pm, ref pl);

        return new BotMove
        {
            Move = move,
            PlanPlacement = pm.Take((int)pl).ToList(),
            PollStatus = status
        };
    }

    /// <summary>
    /// Resets the playfield, back-to-back status, and combo count.
    /// This should only be used when garbage is received or when your client could not place the
    /// piece in the correct position for some reason (e.g. 15 move rule), since this forces the
    /// bot to throw away previous computations.
    /// <b>Note:</b> combo is not the same as the displayed combo in guideline games. Here, it is the
    /// number of consecutive line clears achieved. So, generally speaking, if "x Combo" appears
    /// on the screen, you need to use x+1 here.
    /// </summary>
    /// <param name="board">an array of 400 booleans in row major order, with index 0 being the bottom-left cell.</param>
    /// <param name="combo"></param>
    /// <param name="backToBack"></param>
    public void Reset(bool[] board, int combo, bool backToBack)
    {
        ColdClearInterop.ResetAsync(_bot, board.Select(b => b ? (byte)1 : (byte)0).ToArray(), backToBack, (uint) combo);
    }

    /// <summary>
    /// This function is the same as <see cref="PollNextMove"/> except when <see cref="PollNextMove"/> would return
    /// `Waiting` it instead waits until the bot has made a decision.
    ///
    /// If the move has been provided, this function will return `MoveProvided`.
    /// If the bot has found that it cannot survive, this function will return `BotDead`
    /// </summary>
    /// <param name="move"></param>
    /// <param name="plan"></param>
    /// <param name="planLength"></param>
    /// <returns></returns>
    public BotPollStatus BlockNextMove(out Move move, PlanPlacement[] plan, ref uint planLength)
    {
        return ColdClearInterop.BlockNextMove(_bot, out move, plan, ref planLength);
    }

    public BotMove BlockNextMove(int planLength = 0)
    {
        Move move = new Move();
        var pl = (uint)planLength;
        var pm = new PlanPlacement[planLength == 0 ? 1 : planLength];

        var status = ColdClearInterop.BlockNextMove(_bot, out move, pm, ref pl);

        return new BotMove
        {
            Move = move,
            PlanPlacement = pm.Take((int)pl).ToList(),
            PollStatus = status
        };
    }

    private void ReleaseUnmanagedResources()
    {
        if (_book != null)
            _book.Dispose();

        if (_bot == IntPtr.Zero)
            return;

        ColdClearInterop.DestroyAsync(_bot);
        _bot = IntPtr.Zero;
    }

    public void Dispose()
    {
        ReleaseUnmanagedResources();
        GC.SuppressFinalize(this);
    }

    ~ColdClear()
    {
        ReleaseUnmanagedResources();
    }
}