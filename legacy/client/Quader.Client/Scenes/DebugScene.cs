using Quader.Components.Debugging;

namespace Quader.Scenes;

public class DebugScene : SceneBase
{
    public DebugScene()
    {
        
    }

    public override void Initialize()
    {
        base.Initialize();

        CreateEntity("debug")
            .AddComponent(new DebugRendererComponent());
    }
}