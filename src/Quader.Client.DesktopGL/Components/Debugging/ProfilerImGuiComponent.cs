using System.Collections.Generic;
using System.Diagnostics;
using ImGuiNET;
using Nez;
using Nez.ImGuiTools;
using Quader.Engine;
using Quader.Engine.Utils;

namespace Quader.Components.Debugging;

public class ProfilerImGuiComponent : Component
{
    private readonly Dictionary<string, float[]> _timeCache = new();

    private bool _isBreak = false;

    public ProfilerImGuiComponent()
    {

    }

    public override void OnAddedToEntity()
    {
        Core.GetGlobalManager<ImGuiManager>().RegisterDrawCommand(Draw);
    }

    private void Draw()
    {
        ImGui.Begin("Profiling");

        ImGui.Text($"Delta Time: {Time.DeltaTime:F6}. Time Scale: {Time.TimeScale:F1}. Frame Count: {Time.FrameCount}. Total Time: {Time.TotalTime:F2}.");

        ImGui.Separator();

        foreach (var timeData in GlobalTimeManager.TimeData)
        {
            if (!_timeCache.ContainsKey(timeData.Key))
                _timeCache[timeData.Key] = new float[timeData.Value.MaxSampleSize];

            if (!_isBreak)
                _timeCache[timeData.Key][timeData.Value.CurrentIndex] = (float)timeData.Value.LastTime.TotalMilliseconds;

            //ImGui.Text($"{timeData.Key}: {timeData.Value.LastTime.TotalMilliseconds:F8}");
            ImGui.Text($"{timeData.Value}");
            ImGui.PlotLines(
                $"##{timeData.Value}",
                ref _timeCache[timeData.Key][0],
                timeData.Value.MaxSampleSize,
                timeData.Value.CurrentIndex,
                "",
                0.001f,
                1f,
                new System.Numerics.Vector2(400, 50)
            );
        }

        if (ImGui.Button("Break"))
        {
            Debugger.Break();
        }

        ImGui.End();
    }
}