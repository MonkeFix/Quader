using Microsoft.Xna.Framework.Input;
using Nez;

namespace Quader.Client.Managers;

public class PauseManager : GlobalManager
{
    public bool IsPaused { get; set; }
    public bool PauseOnEscape = true;

    public float TimeScaleBeforePause = Time.TimeScale;

    public override void Update()
    {
        if (PauseOnEscape && Input.IsKeyPressed(Keys.Escape))
        {
            IsPaused = !IsPaused;
        }

        if (IsPaused)
        {
            TimeScaleBeforePause = Time.TimeScale;
            Time.TimeScale = 0.0f;
        }
        else
        {
            Time.TimeScale = 1;
        }
    }
}