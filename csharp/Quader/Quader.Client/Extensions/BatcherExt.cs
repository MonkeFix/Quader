using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Nez;
using Nez.Textures;

namespace Quader.Client.Extensions;

public static class BatcherExt
{
    public static void Draw(this Batcher batcher, Sprite sprite, Vector2 position)
    {
        batcher.Draw(sprite, position, Color.White, 0, sprite.Origin, 1, SpriteEffects.None, 0);
    }
}