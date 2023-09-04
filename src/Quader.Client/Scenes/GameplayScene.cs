using System;
using LiteLog.Logging;
using Microsoft.Xna.Framework;
using Nez;
using Quader.Components;
using Quader.Components.UI;

namespace Quader.Scenes
{
    public class GameplayScene : Scene
    {
        public const int ScreenSpaceRenderLayer = 999;
        public const int BoardTag = 1000;

        public readonly int Width = 1600;
        public readonly int Height = 980;

        private readonly ILogger _logger = LoggerFactory.GetLogger<GameplayScene>();

        private BoardManagerComponent _boardManager;
        private SharedActions _sharedActions;

        public GameplayScene()
        {
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

            var mw = Screen.MonitorWidth;
            var mh = Screen.MonitorHeight;

            SetDesignResolution(mw, mh, SceneResolutionPolicy.BestFit);
            Screen.SetSize(mw, mh);

            /*var ui = new Entity("ui");
            var canvas = ui.AddComponent(new UICanvas());

            var table = canvas.Stage.AddElement(new Table());
            var b = table.Add(new TextButton("Hello!", skin));
            b.Width(256);
            b.Height(64);

            AddEntity(ui);*/

            _sharedActions = new SharedActions();
            _sharedActions.OpenReplaysAction = () =>
            {
                var t = Core.StartSceneTransition(new FadeTransition(() => new ReplayScene(_sharedActions)));
            };

            CreateEntity("shared-ui").AddComponent(new SharedUiComponent(_sharedActions));

            _logger.Debug("Creating Board Entity...");

            _boardManager = CreateEntity("board-manager")
                .AddComponent(new BoardManagerComponent(_sharedActions));

            /*Core.Schedule(2f, true, boardBot, (timer) =>
            {
                timer.GetContext<Board>().PushGarbage(1);
            });

            _logger.Debug("Done initializing");*/
        }
    }
}