using Microsoft.Xna.Framework;
using Nez;
using Quader.Client.Components;

namespace Quader.Client.Scenes;

public class GameplayScene : Scene
{
    public const int BgLayer = 15;

    public const int ScreenSpaceRenderLayer = 999;

    private Renderer _bgRenderer = null!;
    private ScreenSpaceRenderer _screenSpaceRenderer = null!;


    public override void Initialize()
    {
        base.Initialize();

        var screenSizeW = Screen.MonitorWidth;
        var screenSizeH = Screen.MonitorHeight - 128;

        SetDesignResolution(
            (int)(screenSizeW),
            (int)(screenSizeH),
            SceneResolutionPolicy.ShowAllPixelPerfect);
        Screen.SetSize(screenSizeW, screenSizeH);

        _bgRenderer = AddRenderer(new RenderLayerRenderer(-1, BgLayer, -1));
        AddRenderer(new RenderLayerExcludeRenderer(1, BgLayer, -1, ScreenSpaceRenderLayer));
        _screenSpaceRenderer = AddRenderer(new ScreenSpaceRenderer(100, ScreenSpaceRenderLayer));

        var test = CreateEntity("test", new Vector2(300, 100)).AddComponent(new TestComponent());
    }
}