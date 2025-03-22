using Microsoft.Xna.Framework;
using Nez;
using Nez.UI;
using Quader.Client.Skins;

namespace Quader.Client.Components;

public class TestComponent : RenderableComponent
{
    public override float Width => 1000;
    public override float Height => 1000;

    private BoardSkin _boardSkin = null!;

    public override void OnAddedToEntity()
    {
        _boardSkin = Core.Services.GetService<Skin>().Get<BoardSkin>();
    }

    public override void Render(Batcher batcher, Camera camera)
    {
        batcher.Draw(_boardSkin.BoardTexture, Entity.Position);

        var pieceSprites = _boardSkin.PieceTextures.Values.ToList();

        for (int i = 0; i < pieceSprites.Count; i++)
        {
            var tex = pieceSprites[i];
            batcher.Draw(tex, Entity.Position + new Vector2(i * 128, 0));
        }
    }
}