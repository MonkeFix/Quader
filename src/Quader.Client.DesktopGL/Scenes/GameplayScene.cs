using System;
using System.IO;
using Microsoft.Xna.Framework;
using Nez;
using Nez.ImGuiTools;
using Nez.Persistence;
using Nez.Sprites;
using Nez.Timers;
using Nez.UI;
using Quader.Components;
using Quader.Debugging.Logging;
using Quader.Engine;
using Quader.Engine.PieceGenerators;
using Quader.Engine.Pieces;
using Quader.Engine.Settings;
using Quader.Serialization;
using Quader.Skinning;

namespace Quader.Scenes
{
    public class GameplayScene : Scene
    {
        public const int ScreenSpaceRenderLayer = 999;

        public readonly int Width = 1600;
        public readonly int Height = 980;

        private readonly ILogger _logger = LoggerFactory.GetLogger<GameplayScene>();

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

            var skin = Core.Services.GetService<Skin>();

            /*var ui = new Entity("ui");
            var canvas = ui.AddComponent(new UICanvas());

            var table = canvas.Stage.AddElement(new Table());
            var b = table.Add(new TextButton("Hello!", skin));
            b.Width(256);
            b.Height(64);

            AddEntity(ui);*/

            _logger.Debug("Creating Board Entity...");

            var gameSettings = GameSettings.Default;
            

            var queueSize = 5;
            var pieceGenerator = new PieceGeneratorBag7(queueSize);

            var boardSkin = skin.Get<BoardSkin>();
            var gameStateMachine = CreateEntity("game_state_machine").AddComponent(new GameStateMachineComponent());

            var boardPlayerEntity =
                new BoardFactory()
                    .AddGameSettings(gameSettings)
                    .AddPieceHandler(PieceHandlerType.Player)
                    .SetPosition(new Vector2(200, 128))
                    .Build(out var boardPlayer);

            var boardBotEntity =
                new BoardFactory()
                    .AddGameSettings(gameSettings)
                    .AddPieceHandler(PieceHandlerType.Bot)
                    .SetPosition(new Vector2(256 + 512 + 128 + 64, 128))
                    .AddPvpController(boardPlayer)
                    .Build(out var boardBot);

            AddEntity(boardPlayerEntity);
            AddEntity(boardBotEntity);
            
            
            Core.Schedule(2f, true, boardBot, (timer) =>
            {
                timer.GetContext<Board>().PushGarbage(1);
            });

            _logger.Debug("Done initializing");
        }
        
        public override void Update()
        {
            base.Update();
        }
    }
}