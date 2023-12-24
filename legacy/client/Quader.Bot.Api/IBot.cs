using ColdClearNet;

namespace Quader.Bot.Api;

public interface IBot
{
    BotVersion Version { get; }
    BotRunMode RunMode { get; }

    void Start(IEnumerable<Piece> queue, BotRunMode runMode = BotRunMode.Sync, Options? options = null, Weights? weights = null);
    // ReSharper disable once InconsistentNaming
    void Reset(bool[] board, int combo, bool b2b);
    void Destroy();

    BotMove DoMove(int incomingGarbage);
    void PushPiece(Piece piece);
}