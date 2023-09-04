namespace Quader.Engine.Settings;

public class GravitySettings
{
    /// <summary>
    /// Gets or sets constant gravity. If zero, <see cref="BaseGravity"/> used instead.
    /// </summary>
    public float ConstantGravity = 0;
    /// <summary>
    /// Gets or sets starting gravity
    /// </summary>
    public float BaseGravity = 0.8f;
    /// <summary>
    /// Gets or sets the speed with which gravity increases
    /// </summary>
    public float GravityIncrease = 0.007f;

    /// <summary>
    /// Gets or sets number of seconds until a piece locks down when touching the ground
    /// </summary>
    public float LockDelay = 1;
}