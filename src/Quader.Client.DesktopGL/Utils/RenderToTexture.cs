using System;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Nez;

namespace Quader.Utils
{
    public static class RenderToTexture
    {
        public static void RenderFrom(this RenderTarget2D renderTarget, Action<Batcher> renderAction, Color? clearColor = null)
        {
            if (clearColor == null)
                clearColor = Color.Transparent;

            Core.GraphicsDevice.SetRenderTarget(renderTarget);
            Core.GraphicsDevice.Clear(clearColor.Value);

            Graphics.Instance.Batcher.Begin();

            renderAction(Graphics.Instance.Batcher);

            Graphics.Instance.Batcher.End();
        }
    }
}