using System;
using System.IO;
using Microsoft.Xna.Framework;
using Nez;
using Nez.ImGuiTools;
using Nez.Persistence;
using Nez.Sprites;
using Nez.Timers;
using Nez.UI;
using Quader.Components.Boards;
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
            AddRenderer(new DefaultRenderer(100));
            //AddRenderer(new ScreenSpaceRenderer(100, ScreenSpaceRenderLayer));
            AddRenderer(new RenderLayerExcludeRenderer(0, ScreenSpaceRenderLayer));
        }

        public override void Initialize()
        {
            base.Initialize();
            ClearColor = Color.Black; 
            /*var t = new SquaresTransition();
            Core.StartSceneTransition(t);*/

            _logger.Debug("Initializing");

            SetDesignResolution(Width, Height, SceneResolutionPolicy.BestFit);
            Screen.SetSize(1920, 1080);


            var ui = new Entity("ui");
            var canvas = ui.AddComponent(new UICanvas());

            var skin = Core.Services.GetService<Skin>();

            var table = canvas.Stage.AddElement(new Table());
            var b = table.Add(new TextButton("Hello!", skin));
            b.Width(256);
            b.Height(64);

            AddEntity(ui);

            _logger.Debug("Creating Board Entity...");

            var gameSettings = GameSettings.Default;
            

            var queueSize = 5;
            var pieceGenerator = new PieceGeneratorBag7(queueSize);

            var boardSkin = skin.Get<BoardSkin>();

            var boardEntity = CreateBoard("board-player", pieceGenerator);

            boardEntity.Item1.Position = new Vector2(200, 128);


            var pg2 = new PieceGeneratorBag7(queueSize);

            var boardBot = new Board(gameSettings);
            boardBot.SetPiece(PieceType.T);
            
            
            var boardEntityBot = new Entity("board-bot");
            var br2 = boardEntityBot.AddComponent(new SpriteRenderer(boardSkin.BoardTexture));

            Component[] comps = new Component[]
            {
                new BoardGridRendererComponent(boardBot),
                new BoardRendererComponent(boardBot),
                new PieceRendererComponent(boardBot),
                new PieceQueueComponent(boardBot, pg2),
                new HeldPieceComponent(boardBot),
                new ScoreHandlerComponent(boardBot),
                new BoardGravityComponent(boardBot),
                new PieceHandlerBotComponent(boardBot),
            };

            boardEntityBot.AddComponents(comps);

            void RestartAction()
            {
                foreach (var component in comps)
                {
                    if (component is IResetable boardComponent) boardComponent.Reset();
                }
            }

            boardEntityBot.AddComponent(new PvpControllerComponent(boardBot, boardEntity.Item2));
            boardEntityBot.AddComponent(new LoseHandlerComponent(boardBot, RestartAction));

            boardEntityBot.Position = new Vector2(256 + 512 + 128 + 64, 128);
            br2.Origin = new Vector2(188, 0);

            AddEntity(boardEntity.Item1);
            AddEntity(boardEntityBot);

            /*Core.Schedule(.1f, true, boardBot, (timer) =>
            {
                timer.GetContext<Board>().PushGarbage(1);
            });*/

            _logger.Debug("Done initializing");
        }
        
        public override void Update()
        {
            base.Update();
        }

        private (Entity, Board) CreateBoard(string name, IPieceGenerator pieceGenerator)
        {
            var gameSettings = GameSettings.Default;

            var board = new Board(gameSettings);

            var boardSkin = Core.Services.GetService<Skin>().Get<BoardSkin>();

            var boardEntity = new Entity(name);
            var br = boardEntity.AddComponent(new SpriteRenderer(boardSkin.BoardTexture));
            br.Origin = new Vector2(188, 0);

            Component[] comps = {
                new BoardGridRendererComponent(board),
                new BoardRendererComponent(board),
                new PieceRendererComponent(board),
                new BoardImGuiComponent(board),
                new PieceQueueComponent(board, pieceGenerator),
                new HeldPieceComponent(board),
                new ScoreHandlerComponent(board),
                new BoardGravityComponent(board)
            };

            boardEntity.AddComponents(comps);

            void RestartAction()
            {
                foreach (var component in comps)
                {
                    if (component is IResetable boardComponent) boardComponent.Reset();
                }
            }

            boardEntity.AddComponent(new PieceHandlerPlayerComponent(board, RestartAction));
            boardEntity.AddComponent(new LoseHandlerComponent(board, RestartAction));

            return (boardEntity, board);
        }
    }
}