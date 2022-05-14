using System;
using System.Collections.Generic;
using System.IO;
using System.Threading.Tasks;
using Microsoft.Xna.Framework;
using Nez;
using Nez.Systems;
using Nez.UI;
using Quader.Audio;
using Quader.Config;
using Quader.Debugging.Logging;
using Quader.Debugging.Logging.Loggers;
using Quader.Scenes;
using Quader.Skinning;

namespace Quader
{
    public class GameRoot : Core
    {
        private readonly string _configFilePath = "config.json";

        public GameRoot()
            : base(windowTitle: "Quader")
        {
            Window.AllowUserResizing = false;

            LoggerFactory.DefaultLoggers = new List<ILoggerFrontend>
            {
                new ConsoleLogger(),
                new DiagnosticsLogger(),
                new DrawableLogger(Color.Black, 10f, 3f),
                new FileLogger()
            };
            
            IsFixedTimeStep = false;
            TargetElapsedTime = TimeSpan.FromSeconds(1.0 / 240.0);

            FMODManager.Init(FMODMode.Core, "Content");
        }
        
        protected override void Initialize()
        {
            base.Initialize();

            Skin skin = Skin.CreateDefaultSkin();

            var skinTexture = Content.LoadTexture("skins/default_3");
            var boardTexture = Content.LoadTexture("skins/board_default");
            skin.Add("board_skin", new BoardSkin(skinTexture, boardTexture));
            Services.AddService(typeof(Skin), skin);


            GameConfig gc;

            try
            {
                gc = GameConfig.LoadFromFile(_configFilePath);
            }
            catch (FileNotFoundException e)
            {
                Console.WriteLine("Config file was not found, taking the defaults");
                gc = new GameConfig();
            }
            catch (Exception e)
            {
                throw;
            }

            Services.AddService(gc);

            Scene = new GameplayScene();
        }

        protected override void LoadContent()
        {
            base.LoadContent();
        }

        protected override void UnloadContent()
        {
            base.UnloadContent();

            var gc = Services.GetService<GameConfig>();
            GameConfig.SaveToFile(gc, _configFilePath);
        }

        protected override void Update(GameTime gameTime)
        {
            base.Update(gameTime);
        }
    }
}
