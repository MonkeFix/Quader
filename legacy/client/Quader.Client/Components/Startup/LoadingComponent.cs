using Microsoft.Xna.Framework;
using Nez;

namespace Quader.Components.Startup;

public class LoadingComponent : RenderableComponent
{
    public override float Width => 2000;
    public override float Height => 2000;

    public override void Render(Batcher batcher, Camera camera)
    {
        batcher.DrawString(Graphics.Instance.BitmapFont, "LOADING...", new Vector2(128, 128), Color.White);
    }
}