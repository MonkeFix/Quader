using Nez;
using Quader.Scenes;

namespace Quader
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
