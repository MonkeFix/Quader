using Nez;
using Quader.Engine;

namespace Quader.Client.Components;

public class TimeManagerComponent : SceneComponent
{
    public TimeManager TimeManager { get; } = new TimeManager();

    public override void Update()
    {
        TimeManager.Update(Time.DeltaTime);
    }
}