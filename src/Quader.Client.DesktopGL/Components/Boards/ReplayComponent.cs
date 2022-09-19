using System;
using Nez;
using Quader.Engine;
using Quader.Engine.Replays;

namespace Quader.Components.Boards;

public class ReplayComponent : Component, IBoardComponent, IBoardToggleable
{
    public Board Board { get; }

    private bool _isEnabled;

    private TimeManagerComponent _timeManager = null!;

    private bool _isStarted;

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

        Board.StartMoveHolder(_timeManager.StartTimeUtc);
        _isStarted = true;
    }

    public BoardMoveHolder End()
    {
        if (!_isStarted)
            throw new Exception("BoardMoveHolder has not been started");

        var replay = Board.StopMoveHolder(DateTime.UtcNow);



        return replay;
    }

    public void Enable()
    {
        _isEnabled = true;
        Start();
    }

    public void Disable()
    {
        _isEnabled = false;
    }
}