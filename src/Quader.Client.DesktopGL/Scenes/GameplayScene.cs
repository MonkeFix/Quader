using System;
using System.IO;
using System.Linq.Expressions;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Nez;
using Nez.ImGuiTools;
using Nez.Persistence;
using Nez.Sprites;
using Nez.UI;
using Quader.Components;
using Quader.Components.Boards;
using Quader.Debugging.Logging;
using Quader.Engine;
using Quader.Engine.PieceGenerators;
using Quader.Engine.Pieces;
using Quader.Skinning;

namespace Quader.Scenes
{
    public class PointJsonConverter : JsonTypeConverter<Point>
    {
        public override bool WantsExclusiveWrite => true;

        public override void WriteJson(IJsonEncoder encoder, Point value)
        {
            encoder.EncodeKeyValuePair("p", new [] { value.X, value.Y });
        }

        public override void OnFoundCustomData(Point instance, string key, object value)
        {
            Console.WriteLine($"filed name: {key}, value: {value}");
        }
    }

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
            
            _logger.Debug("Initializing");

            var imGuiManager = new ImGuiManager();
            Core.RegisterGlobalManager(imGuiManager);
            
            SetDesignResolution(Width, Height, SceneResolutionPolicy.BestFit);
            Screen.SetSize(1920, 1080);

            LoadSrs();

            _logger.Debug("Creating Board Entity...");

            var board = new Board();
            board.SetPiece(PieceType.T);

            var pieceGenerator = new PieceGeneratorBag7(5);

            var boardSkin = Core.Services.GetService<Skin>().Get<BoardSkin>();

            var boardEntity = new Entity("board-player");
            var br = boardEntity.AddComponent(new SpriteRenderer(boardSkin.BoardTexture));
            br.Origin = new Vector2(188, 0);
            boardEntity.AddComponent(new BoardGridRendererComponent(board));
            boardEntity.AddComponent(new BoardRendererComponent(board));
            boardEntity.AddComponent(new PieceRendererComponent(board));
            boardEntity.AddComponent(new PieceHandlerPlayerComponent(board));
            boardEntity.AddComponent(new BoardImGuiComponent(board));
            boardEntity.AddComponent(new PieceQueueComponent(board, pieceGenerator));
            boardEntity.AddComponent(new HeldPieceComponent(board));
            boardEntity.AddComponent(new ScoreHandlerComponent(board));

            boardEntity.Position = new Vector2(200, 128);

            var boardBot = new Board();
            boardBot.SetPiece(PieceType.T);
            var pg2 = new PieceGeneratorBag7(5);
            
            var boardEntityBot = new Entity("board-bot");
            var br2 = boardEntityBot.AddComponent(new SpriteRenderer(boardSkin.BoardTexture));
            
            boardEntityBot.AddComponent(new BoardGridRendererComponent(boardBot));
            boardEntityBot.AddComponent(new BoardRendererComponent(boardBot));
            boardEntityBot.AddComponent(new PieceHandlerBotComponent(boardBot));
            boardEntityBot.AddComponent(new PieceRendererComponent(boardBot, false));
            // boardEntityBot.AddComponent(new PieceHandlerComponent(board));
            //boardEntityBot.AddComponent(new BoardImGuiComponent(boardBot));
            boardEntityBot.AddComponent(new PieceQueueComponent(boardBot, pg2));
            boardEntityBot.AddComponent(new HeldPieceComponent(boardBot));
            boardEntityBot.AddComponent(new ScoreHandlerComponent(boardBot));

            boardEntityBot.Position = new Vector2(256 + 512 + 128 + 64, 128);
            br2.Origin = new Vector2(188, 0);

            AddEntity(boardEntity);
            AddEntity(boardEntityBot);

            _logger.Debug("Done initializing");
        }
        
        public override void Update()
        {
            base.Update();
        }

        private void LoadSrs()
        {
            /*_logger.Debug("Loading Rotation Tables..");

            var data = RotationTableConverter.FromTexture2D(Content.Load<Texture2D>("data/srs_rotations"));

            using (var sw = new StreamWriter("srs.json", false))
            {
                sw.WriteLine(Json.ToJson(data, new JsonSettings
                {
                    PrettyPrint = true,
                    //TypeConverters = new JsonTypeConverter[] {new PointJsonConverter()}
                }));
            }

            RotationSystemTable rst;

            using (var sr = new StreamReader("srs.json"))
            {
                var content = sr.ReadToEnd();
                rst = Json.FromJson<RotationSystemTable>(content);
            }


            using var sr2 = new StreamWriter("srs_table.json");
            sr2.WriteLine(Json.ToJson(
                new RotationTableTests
                {
                    DefaultWallKickData = PieceUtils.DefaultWallKickData,
                    PieceIWallKickData = PieceUtils.PieceIWallKickData,
                    PieceOWallKickData = PieceUtils.PieceOWallKickData
                }
                , true));

            _logger.Debug("Done loading Rotation Tables");*/
        }
    }
}