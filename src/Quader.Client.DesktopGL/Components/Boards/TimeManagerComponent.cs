using System;
using ImGuiNET;
using Nez;
using Nez.ImGuiTools;
using Quader.Debugging.Logging;
using Quader.Engine;
using Quader.Engine.Replays;
using Quader.Managers.TimeProviders;

namespace Quader.Components.Boards;

public class TimeManagerComponent : Component, IBoardComponent, IUpdatable, IBoardToggleable
{
    public Board Board { get; }

    public double ElapsedMilliseconds => _timeProvider.CurrentTimeMs;
    public double ElapsedSeconds => _timeProvider.CurrentTimeMs / 1000;
    public double ElapsedMinutes => _timeProvider.CurrentTimeMs / 1000 / 60;
    public ulong UpdateCycles => _timeProvider.UpdateCycles;
    public float DeltaTime => _timeProvider.DeltaTime;

    public DateTimeOffset StartTimeUtc { get; private set; }

    private readonly ITimeProvider _timeProvider;

    private BoardMoveHolder _boardMoveHolder;
    
    private readonly ILogger _logger = LoggerFactory.GetLogger<TimeManagerComponent>();

    public TimeManagerComponent(Board board, ITimeProvider timeProvider)
    {
        Board = board;
        _timeProvider = timeProvider;
    }

    public override void OnAddedToEntity()
    {
#if DEBUG
        var imGuiManager = Core.GetGlobalManager<ImGuiManager>();
        //if (imGuiManager.Enabled)
        //    imGuiManager.RegisterDrawCommand(Draw);
#endif
    }

    public void Update()
    {
        if (Enabled)
            _timeProvider.Update();

        if (_boardMoveHolder == null)
            _boardMoveHolder = Board.Replay;
    }

    public void Enable()
    {
        var time = DateTimeOffset.UtcNow;
        _logger.Trace($"Enabling at {time}");
        Enabled = true;
        StartTimeUtc = time;
    }

    public void Disable()
    {
        _logger.Trace("Disabling");
        Enabled = false;
    }

    private void Draw()
    {
        return;

        ImGui.Begin("Time Stats Board " + Entity.Name);

        /*
            $"DT: {_timeManager.DeltaTime:F4}\n" +
            $"Start: {_timeManager.StartTimeUtc}\n" +
            $"UC: {_timeManager.UpdateCycles}\n" +
            $"MS: {_timeManager.ElapsedMilliseconds:F0}\n" +
            $"Sec: {_timeManager.ElapsedSeconds:F3}\n" +
            $"Min: {_timeManager.ElapsedMinutes:F2}";
        */

        ImGui.Text($"DeltaTime: {DeltaTime}");
        ImGui.Text($"Start Time UTC: {StartTimeUtc}"); 
        ImGui.Text($"Update Cycles: {UpdateCycles}");
        ImGui.Text($"Elapsed ms: {ElapsedMilliseconds}");
        ImGui.Text($"Elapsed sec: {ElapsedSeconds}");
        ImGui.Text($"Elapsed min: {ElapsedMinutes}");

        ImGui.Separator();

        if (_boardMoveHolder != null)
        {
            ImGui.Text($"REPLAY:");
            ImGui.Text($"Start Time UTC: {_boardMoveHolder.StartDate}");
            ImGui.Text($"Moves:");

            /*foreach (var replayMove in _boardMoveHolder.Moves)
            {
                 ImGui.Text(
                    $"Type: {replayMove.Type}, Tick: {replayMove.Tick}, Board Move: {replayMove.BoardHardDropInfo}, LF: {replayMove.Info.MoveLeftFactor}, RF: {replayMove.Info.MoveRightFactor}, SDF: {replayMove.Info.SoftDropFactor}");
            }*/
        }

        ImGui.End();
    }
}