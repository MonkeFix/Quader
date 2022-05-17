using Nez.Persistence;

namespace Quader.Engine.Settings;

public class GravitySettings
{
    /// <summary>
    /// Gets or sets constant gravity. If zero, <see cref="BaseGravity"/> used instead.
    /// </summary>
    [JsonInclude]
    public float ConstantGravity { get; set; } = 0;
    /// <summary>
    /// Gets or sets starting gravity
    /// </summary>
    [JsonInclude]
    public float BaseGravity { get; set; } = 0.8f;
    /// <summary>
    /// Gets or sets the speed with which gravity increases
    /// </summary>
    [JsonInclude]
    public float GravityIncrease { get; set; } = 0.007f;

    /// <summary>
    /// Gets or sets number of seconds until a piece locks down when touching the ground
    /// </summary>
    [JsonInclude]
    public float LockDelay { get; set; } = 1;
}