using System;
using System.Collections.Generic;
using System.IO;
using System.Threading;
using ColdClearNet;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Input;
using Nez;
using Nez.ImGuiTools;
using Nez.Persistence;
using Nez.Sprites;
using Nez.Timers;
using Nez.UI;
using Quader.Components;
using Quader.Components.UI;
using Quader.Debugging.Logging;
using Quader.Engine;
using Quader.Engine.PieceGenerators;
using Quader.Engine.Pieces;
using Quader.Engine.Replays;
using Quader.Engine.Settings;
using Quader.Managers;
using Quader.Serialization;
using Quader.Skinning;

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

            SetDesignResolution(Width, Height, SceneResolutionPolicy.BestFit);
            Screen.SetSize(1920, 1080);

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