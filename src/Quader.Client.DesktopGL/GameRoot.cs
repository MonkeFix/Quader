using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using Microsoft.Xna.Framework;
using Nez;
using Nez.Systems;
using Nez.UI;
using Quader.Debugging.Logging;
using Quader.Debugging.Logging.Loggers;
using Quader.Scenes;
using Quader.Skinning;

namespace Quader
{
    public class GameRoot : Core
    {
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
        }
        
        protected override void Initialize()
        {
            base.Initialize();

            Skin skin = Skin.CreateDefaultSkin();

            var skinTexture = Content.LoadTexture("skins/default");
            skin.Add("board_skin", new BoardSkin(skinTexture));
            Services.AddService(typeof(Skin), skin);

            Scene = new GameplayScene();
        }

        protected override void LoadContent()
        {
            base.LoadContent();
        }

        protected override void Update(GameTime gameTime)
        {
            base.Update(gameTime);
        }
    }
}
