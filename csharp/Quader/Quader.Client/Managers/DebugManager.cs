using Microsoft.Xna.Framework.Input;
using Nez.ImGuiTools;
using Nez;

namespace Quader.Client.Managers;

public class DebugManager : GlobalManager
{
    private PauseManager _pauseManager;

    public DebugManager(PauseManager pauseManager)
    {
        _pauseManager = pauseManager;
    }

    public override void Update()
    {
        if (Input.IsKeyDown(Keys.LeftControl) || Input.IsKeyDown(Keys.RightControl))
        {
            if (Input.IsKeyPressed(Keys.B))
            {
                Core.DebugRenderEnabled = !Core.DebugRenderEnabled;
            }

#if DEBUG
            if (Input.IsKeyPressed(Keys.N))
            {
                ImGuiManager.ToggleImGui();
            }
#endif

            if (Input.IsKeyPressed(Keys.V))
            {
                //Time.TimeScale = Time.TimeScale > 0 ? 0 : 1;
                _pauseManager.IsPaused = !_pauseManager.IsPaused;
            }
        }
    }
}