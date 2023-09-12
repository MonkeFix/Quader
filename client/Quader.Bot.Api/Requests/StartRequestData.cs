using ColdClearNet;

namespace Quader.Bot.Api.Requests;

public class StartRequestData
{
    public Piece[] Queue = null!;
    public BotRunMode RunMode = BotRunMode.Sync;
    public Options? Options;
    public Weights? Weights;
}