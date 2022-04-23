using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Microsoft.Xna.Framework.Input;
using Nez;
using Tetramonio.Scenes;

namespace Tetramonio
{
    public class GameRoot : Core
    {

        public GameRoot()
        {
            Window.AllowUserResizing = false;
        }
        
        protected override void Initialize()
        {
            base.Initialize();

            Scene = new GameplayScene();
        }
    }
}
