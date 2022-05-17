using System;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Nez;

namespace Quader.Utils
{
    public static class RenderTarget2DExtensions
    {
        public static void RenderFrom(this RenderTarget2D renderTarget, Batcher batcher, Action<Batcher> renderAction,
            Color? clearColor = null)
        {
            if (clearColor == null)
                clearColor = Color.Transparent;

            Core.GraphicsDevice.SetRenderTarget(renderTarget);
            Core.GraphicsDevice.Clear(clearColor.Value);

            batcher.Begin();

            renderAction(batcher);

            batcher.End();
        }

        public static void RenderFrom(this RenderTarget2D renderTarget, Action<Batcher> renderAction, Color? clearColor = null)
        {
           RenderFrom(renderTarget, Graphics.Instance.Batcher, renderAction, clearColor);
        }
    }
}