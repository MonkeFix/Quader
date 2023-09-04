using Microsoft.Xna.Framework.Input;
using Nez.Persistence;

namespace Quader.Config;

public class Controls : IGameConfigEntry<Controls>
{
    [JsonInclude] public Keys MoveLeft { get; set; } = Keys.Left;
    [JsonInclude] public Keys MoveRight { get; set; } = Keys.Right;
    [JsonInclude] public Keys RotateClockwise { get; set; } = Keys.X;
    [JsonInclude] public Keys RotateCounterClockwise { get; set; } = Keys.Z;
    [JsonInclude] public Keys SoftDrop { get; set; } = Keys.Down;
    [JsonInclude] public Keys HardDrop { get; set; } = Keys.Space;
    [JsonInclude] public Keys Rotate180Deg { get; set; } = Keys.F;
    [JsonInclude] public Keys Hold { get; set; } = Keys.C;
    [JsonInclude] public Keys Restart { get; set; } = Keys.R;

    public Controls Defaults()
    {
        return new Controls();
    }
}