using LiteLog.Logging;
using Microsoft.Xna.Framework;
using Nez;
using Quader.Components.UI;
using Quader.Engine.Replays;

namespace Quader.Scenes;

public class ReplayScene : Scene
{
    public BoardMoveHolder? Replay { get; set; }

    public const int ScreenSpaceRenderLayer = 999;

    public readonly int Width = 1600;
    public readonly int Height = 980;

    private readonly ILogger _logger = LoggerFactory.GetLogger<ReplayScene>();

    private SharedActions _sharedActions;

    public ReplayScene(SharedActions sharedActions)
    {
        _sharedActions = sharedActions;

        _logger.Debug("Constructing, adding renderer");
        AddRenderer(new DefaultRenderer(0));
        AddRenderer(new ScreenSpaceRenderer(100, ScreenSpaceRenderLayer));
        AddRenderer(new RenderLayerExcludeRenderer(0, ScreenSpaceRenderLayer));
    }

    public override void Initialize()
    {
        base.Initialize();
        ClearColor = Color.Black;

        _logger.Debug("Initializing");

        SetDesignResolution(Width, Height, SceneResolutionPolicy.BestFit);
        Screen.SetSize(1920, 1080);

        CreateEntity("shared-ui").AddComponent(new SharedUiComponent(_sharedActions));
    }
}