using System;
using Nez;
using Quader.Debugging.Logging;
using Quader.Engine;
using Quader.Engine.Replays;

namespace Quader.Components.Boards;

public class ReplayComponent : Component, IBoardComponent, IBoardToggleable
{
    public Board Board { get; }

    private bool _isEnabled;

    private TimeManagerComponent _timeManager = null!;

    private bool _isStarted;

    private readonly ILogger _logger = LoggerFactory.GetLogger<ReplayComponent>();

    public ReplayComponent(Board board)
    {
        Board = board;
    }

    public override void OnAddedToEntity()
    {
        _timeManager = Entity.GetComponent<TimeManagerComponent>();
    }

    public void Start()
    {
        if (_isStarted)
            return;
        
        _logger.Trace($"Staring at {_timeManager.StartTimeUtc}");

        Board.StartMoveHolder(_timeManager.StartTimeUtc);
        _isStarted = true;
    }

    public BoardMoveHolder End()
    {
        if (!_isStarted)
            throw new Exception("BoardMoveHolder has not been started");

        var endTime = DateTimeOffset.UtcNow;
        _logger.Trace($"Ending at {endTime}");
        var replay = Board.StopMoveHolder(endTime);
        
        return replay;
    }

    public void Enable()
    {
        _logger.Trace("Enabling");
        _isEnabled = true;
        Start();
    }

    public void Disable()
    {
        _logger.Trace("Disabling");
        _isEnabled = false;
    }
}