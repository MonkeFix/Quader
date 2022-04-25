using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Nez;

namespace Quader.Components
{
    public class TestComponent : RenderableComponent
    {
        public override float Width => 1000;
        public override float Height => 1000;

        private Texture2D _blockTexture;

        public override void Initialize()
        {
            base.Initialize();

            _blockTexture = Core.Content.Load<Texture2D>("skins/default");
        }

        public override void Render(Batcher batcher, Camera camera)
        {
            batcher.Draw(_blockTexture, new Vector2(64, 64), Color.White);
        }
    }
}