using System;

namespace Quader.Components.UI;

public class SharedActions
{
    public Action NewGameAction { get; set; } = null!;
    public Action RestartAction { get; set; } = null!;
    public Action QuitAction { get; set; } = null!;
    public Action OpenReplaysAction { get; set; } = null!;

    public bool ClearImmediately { get; set; } = false;
}