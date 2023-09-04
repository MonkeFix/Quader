using System;
using Nez;

namespace Quader;

public static class InputHandler
{
    /// <summary>
    /// The speed at which pieces move when holding down movement keys, measured in milliseconds
    /// </summary>
    public static float AutomaticRepeatRate  = 0f;

    /// <summary>
    /// The time between the initial keypress and the start of its automatic repeat movement
    /// </summary>
    public static float DelayedAutoShift = 128f;

    /// <summary>
    /// If not 0, any ongoing DAS movement will pause for a set amount of time after dropping/rotating a piece 
    /// </summary>
    public static float DasCutDelay = 0f;

    /// <summary>
    /// The factor with which soft drops change the gravity speed
    /// </summary>
    public static float SoftDropFactor = Single.PositiveInfinity;
        
    public static void Update()
    {
        var dt = Time.DeltaTime;
            
            
    }
}