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

        public readonly int Width = 1920;
        public readonly int Height = 1080;

        private readonly ILogger _logger = LoggerFactory.GetLogger<GameplayScene>();

        public GameplayScene()
        {
            AddRenderer(new ScreenSpaceRenderer(100, ScreenSpaceRenderLayer));
            AddRenderer(new RenderLayerExcludeRenderer(0, ScreenSpaceRenderLayer));
        }

        public override void Initialize()
        {
            base.Initialize();

            _logger.Trace("Initializing");
            _logger.Debug("Initializing");
            _logger.Info("Initializing");
            _logger.Warn("Initializing");
            _logger.Error("Initializing");
            _logger.Critical("Initializing");


            var imGuiManager = new ImGuiManager();
            Core.RegisterGlobalManager(imGuiManager);
            
            SetDesignResolution(Width, Height, SceneResolutionPolicy.ShowAllPixelPerfect);
            Screen.SetSize(Width, Height);

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
            

            /*var e = new Entity("test").AddComponent<TestComponent>();

            AddEntity(e.Entity);*/

            var e2 = new Entity("board").AddComponent<BoardComponent>();
            AddEntity(e2.Entity);
        }

        public override void Update()
        {
            base.Update();
        }
    }
}