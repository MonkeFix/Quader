using Microsoft.Xna.Framework;
using Nez;
using Quader.Client.Components;
using Quader.Client.Components.Boards;
using Quader.Client.Components.Boards.Controllers;
using Quader.Engine;
using Quader.Engine.Boards;
using Quader.Engine.Pieces.WallKick;

namespace Quader.Client.Scenes;

public class GameplayScene : Scene
{
    public const int BgLayer = 15;

    public const int ScreenSpaceRenderLayer = 999;

    private Renderer _bgRenderer = null!;
    private ScreenSpaceRenderer _screenSpaceRenderer = null!;


    public override void Initialize()
    {
        ClearColor = Color.Black;

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

        //var test = CreateEntity("test", new Vector2(300, 100)).AddComponent(new TestComponent());

        // var timeManagerComponent = CreateEntity("time-manager")
        //     .AddComponent(new TimeManagerComponent());
        AddSceneComponent(new TimeManagerComponent());

        var gameSettings = GameSettings.Default;
        gameSettings.Gravity.GravityBase = 0;
        var wkd = new WallKickData();
        var board = new Board(gameSettings, wkd, Environment.TickCount);

        CreateEntity("board1", new Vector2(300, 100))
            .AddComponent(new BoardRenderer(board))
            .AddComponent(new BoardGhostRenderer(board))
            .AddComponent(new CurrentPieceRenderer(board))
            .AddComponent(new BoardUpdater(board))
            .AddComponent(new BoardControllerLocal(board))
            .AddComponent(new BoardQueueRenderer(board))
            .AddComponent(new BoardHoldPieceRenderer(board))
            ;
    }
}