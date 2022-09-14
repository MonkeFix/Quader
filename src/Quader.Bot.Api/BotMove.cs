using ColdClearNet;

namespace Quader.Bot.Api;

public class BotMove
{
    public BotStatus Status { get; }
    public bool IsHoldUsed { get; internal set; }
    public Movement[]? Movements { get; internal set; }
    public int MovementCount { get; internal set; }

    internal BotMove(BotStatus status)
    {
        Status = status;
    }
}