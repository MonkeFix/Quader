using System;
using System.Collections.Generic;
using System.IO;
using System.Threading.Tasks;
using Microsoft.Xna.Framework;
using Nez;
using Nez.Persistence;
using Nez.Systems;
using Nez.UI;
using Quader.Audio;
using Quader.Config;
using Quader.Debugging.Logging;
using Quader.Debugging.Logging.Loggers;
using Quader.Engine.Pieces;
using Quader.Scenes;
using Quader.Serialization;
using Quader.Skinning;

namespace Quader
{
    public class GameRoot : Core
    {
        private readonly string _configFilePath = "config.json";

        public static JsonSettings DefaultJsonSettings { get; private set; } = null!;

        public GameRoot()
            : base(windowTitle: "Quader")
        {
            Window.AllowUserResizing = false;

            IsFixedTimeStep = false;
            TargetElapsedTime = TimeSpan.FromSeconds(1.0 / 240.0);

            FMODManager.Init(FMODMode.Core, "Content");

            DefaultJsonSettings = new JsonSettings
            {
                TypeConverters = new JsonTypeConverter[]
                {
                    new ColorJsonConverter(),
                    new ColorFactoryConverter()
                },
                PrettyPrint = true
            };

            var pieceSettingsFilename = Path.Combine("Content", "data", "default_piece_settings.json");
            PieceSettings pieceSettings;

            if (!File.Exists(pieceSettingsFilename))
            {
                using var sw = new StreamWriter(pieceSettingsFilename, false);
                pieceSettings = new PieceSettings();
                sw.WriteLine(Json.ToJson(pieceSettings, DefaultJsonSettings));
            }
            else
            {
                using var sr = new StreamReader(pieceSettingsFilename);
                var json = sr.ReadToEnd();
                pieceSettings = Json.FromJson<PieceSettings>(json, DefaultJsonSettings);
            }

            PieceUtils.PieceSettings = pieceSettings;
        }
        
        protected override void Initialize()
        {
            base.Initialize();

            Skin skin = Skin.CreateDefaultSkin();

            var skinTexture = Content.LoadTexture("Content/skins/default_3.png");
            var boardTexture = Content.LoadTexture("Content/skins/board_default.png");
            skin.Add("board_skin", new BoardSkin(skinTexture, boardTexture));
            Services.AddService(typeof(Skin), skin);

            /*var sound = CoreSystem.LoadStreamedSound("test.mp3");
            var channel = sound.Play();
            channel.Volume = 0.1f;
            channel.Looping = true;*/

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

            FMODManager.Unload();
        }

        protected override void Update(GameTime gameTime)
        {
            base.Update(gameTime);

            FMODManager.Update();
        }
    }
}
