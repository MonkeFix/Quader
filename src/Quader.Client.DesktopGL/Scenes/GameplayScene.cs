using System;
using System.IO;
using System.Linq.Expressions;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Nez;
using Nez.ImGuiTools;
using Nez.Persistence;
using Quader.Components;
using Quader.Debugging.Logging;
using Quader.Engine;
using Quader.Engine.Pieces;
using Quader.Engine.RotationEncoder;

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

        public readonly int Width = 1440;
        public readonly int Height = 980;

        private readonly ILogger _logger = LoggerFactory.GetLogger<GameplayScene>();

        public GameplayScene()
        {
            AddRenderer(new ScreenSpaceRenderer(100, ScreenSpaceRenderLayer));
            AddRenderer(new RenderLayerExcludeRenderer(0, ScreenSpaceRenderLayer));
        }

        public override void Initialize()
        {
            base.Initialize();

            //_logger.Trace("Initializing");
            _logger.Debug("Initializing");
            /*_logger.Info("Initializing");
            _logger.Warn("Initializing");
            _logger.Error("Initializing");
            _logger.Critical("Initializing");*/


            var imGuiManager = new ImGuiManager();
            Core.RegisterGlobalManager(imGuiManager);
            
            SetDesignResolution(Width, Height, SceneResolutionPolicy.BestFit);
            Screen.SetSize(1920, 1080);

            LoadSrs();

            var board = new Board();
            board.PushPiece(PieceType.T);

            var boardEntity = new Entity("board-player");
            boardEntity.AddComponent(new BoardRendererComponent(board));
            boardEntity.AddComponent(new PieceRendererComponent(board));
            boardEntity.AddComponent(new PieceHandlerComponent(board));
            boardEntity.AddComponent(new BoardImGuiComponent(board));
            
            boardEntity.Position = new Vector2(128, -460);

            AddEntity(boardEntity);
        }

        public override void Update()
        {
            base.Update();
        }

        private void LoadSrs()
        {
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
            sr2.WriteLine(Json.ToJson(new
            {
                DefaultPiece = PieceUtils.DefaultWallKickData,
                IPiece = PieceUtils.PieceIWallKickData,
                OPiece = PieceUtils.PieceOWallKickData
            }));
        }
    }
}