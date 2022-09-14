using ColdClearNet;

namespace Quader.Bot.Api.Requests;

public class BotMovementRequestData
{
    public BotStatus Status;
    public bool IsHoldUsed;
    public int MovementCount;
    public Movement[]? Movements;
}