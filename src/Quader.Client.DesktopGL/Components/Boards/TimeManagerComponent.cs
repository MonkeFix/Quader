using System;
using Nez;
using Quader.Engine;
using Quader.Managers.TimeProviders;

namespace Quader.Components.Boards;

public class TimeManagerComponent : Component, IBoardComponent, IUpdatable, IBoardToggleable
{
    public Board Board { get; }

    public double CurrentTimeMs => _timeProvider.CurrentTimeMs;
    public ulong UpdateCycles => _timeProvider.UpdateCycles;
    public float DeltaTime => _timeProvider.DeltaTime;

    public DateTime StartTimeUtc { get; private set; }

    private readonly ITimeProvider _timeProvider;

    public TimeManagerComponent(Board board, ITimeProvider timeProvider)
    {
        Board = board;
        _timeProvider = timeProvider;
    }

    public void Update()
    {
        if (Enabled)
            _timeProvider.Update();
    }

    public void Enable()
    {
        Enabled = true;
        StartTimeUtc = DateTime.UtcNow;
    }

    public void Disable()
    {
        Enabled = false;
    }
}