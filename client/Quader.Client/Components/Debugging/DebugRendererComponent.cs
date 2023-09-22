using Microsoft.Xna.Framework;
using Nez;
using Nez.UI;
using Quader.Engine.Pieces;
using Quader.Engine.Pieces.Impl;
using Quader.Skinning;

namespace Quader.Components.Debugging;

public class DebugRendererComponent : RenderableComponent
{
    public override float Width => 1000;
    public override float Height => 1000;

    private List<PieceBase> _pieces = null!;

    private BoardSkin _skin = null!;

    public override void OnAddedToEntity()
    {
        _skin = Core.Services.GetService<Skin>().Get<BoardSkin>();
        
        _pieces = new List<PieceBase>
        {
            new PieceI(), new PieceJ(), new PieceL(), new PieceO(), new PieceS(), new PieceT(), new PieceZ()       
        };
    }

    public override void Render(Batcher batcher, Camera camera)
    {
        // Pieces:
        for (int i = 0; i < _pieces.Count; i++)
        {
            var p = _pieces[i];
            var basePos = new Vector2(128 * i + 128, 128);
            
            foreach (var pos in p.CurrentPos)
            {
                batcher.DrawHollowRect(
                    new Vector2(basePos.X + pos.X * 32, basePos.Y + pos.Y * 32),
                    32,
                    32,
                    Color.Red
                );
            }

            if (p.OffsetType == OffsetType.Cell)
            {
                batcher.DrawPixel(basePos + Vector2.One * 16, Color.LightBlue, 6);
                batcher.DrawString(_skin.DebugFont, $"{{0, 0}}", basePos + Vector2.One * 17, Color.White);
            }
            else
            {
                batcher.DrawPixel(basePos, Color.LightBlue, 6);
                batcher.DrawString(_skin.DebugFont, $"{{0, 0}}", basePos + Vector2.One * 2, Color.White);
            }
            
        }
        
        // Wall Kick Data
        var vals = PieceSettings.DefaultWallKickData.Values.ToList();
        for (int i = 0; i < vals.Count; i++)
        {
            var points = vals[i];
            var basePos = new Vector2(200 * i + 128, 512);

            foreach (var p in points)
            {
                batcher.DrawHollowRect(
                    new Vector2(basePos.X + p.X * 32, basePos.Y + p.Y * 32),
                    32,
                    32,
                    Color.Red
                );
            }
            
            batcher.DrawPixel(basePos + Vector2.One * 16, Color.LightBlue, 6);
            batcher.DrawString(_skin.DebugFont, $"{{0, 0}}", basePos + Vector2.One * 17, Color.White);
        }
    }
}