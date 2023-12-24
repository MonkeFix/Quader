using LiteLog.Profiling;
using Microsoft.Xna.Framework;
using Nez;
using Nez.UI;
using Quader.Engine.Pieces;
using Quader.Engine.Pieces.Impl;
using Quader.Skinning;
using Point = System.Drawing.Point;

namespace Quader.Components.Debugging;

public class DebugRendererComponent : RenderableComponent
{
    public override float Width => 1000;
    public override float Height => 1000;

    private List<PieceBase> _pieces = null!;

    private BoardSkin _skin = null!;

    private int[][] _pieceJ;
    private int[][] _pieceJ90;
    private int[][] _pieceJ180;
    private int[][] _pieceJ270;


    private int[][] _pieceI;
    private int[][] _pieceI90;
    private int[][] _pieceI180;
    private int[][] _pieceI270;

    public override void OnAddedToEntity()
    {
        _skin = Core.Services.GetService<Skin>().Get<BoardSkin>();
        
        _pieces = new List<PieceBase>
        {
            new PieceI(), new PieceJ(), new PieceL(), new PieceO(), new PieceS(), new PieceT(), new PieceZ()       
        };

        _pieceJ = new []
        {
            new [] { 0, 0, 1 },
            new [] { 1, 1, 1 },
            new [] { 0, 0, 0 },
        };
        _pieceJ90 = new []
        {
            new [] { 0, 0, 1 },
            new [] { 1, 1, 1 },
            new [] { 0, 0, 0 },
        };
        _pieceJ180 = new []
        {
            new [] { 0, 0, 1 },
            new [] { 1, 1, 1 },
            new [] { 0, 0, 0 },
        };
        _pieceJ270 = new []
        {
            new [] { 0, 0, 1 },
            new [] { 1, 1, 1 },
            new [] { 0, 0, 0 },
        };
        
        RotatePiece(_pieceJ90);
        
        RotatePiece(_pieceJ180);
        RotatePiece(_pieceJ180);
        
        RotatePiece(_pieceJ270);
        RotatePiece(_pieceJ270);
        RotatePiece(_pieceJ270);

        PrintMatrix(_pieceJ);
        PrintMatrix(_pieceJ90);
        PrintMatrix(_pieceJ180);
        PrintMatrix(_pieceJ270);


        _pieceI = new[]
        {
            new [] { 0, 0, 0, 0 },
            new [] { 1, 1, 1, 1 },
            new [] { 0, 0, 0, 0 },
            new [] { 0, 0, 0, 0 },
        };
        _pieceI90 = new[]
        {
            new [] { 0, 0, 0, 0 },
            new [] { 1, 1, 1, 1 },
            new [] { 0, 0, 0, 0 },
            new [] { 0, 0, 0, 0 },
        };
        _pieceI180 = new[]
        {
            new [] { 0, 0, 0, 0 },
            new [] { 1, 1, 1, 1 },
            new [] { 0, 0, 0, 0 },
            new [] { 0, 0, 0, 0 },
        };
        _pieceI270 = new[]
        {
            new [] { 0, 0, 0, 0 },
            new [] { 1, 1, 1, 1 },
            new [] { 0, 0, 0, 0 },
            new [] { 0, 0, 0, 0 },
        };

        RotatePiece(_pieceI90);

        RotatePiece(_pieceI180);
        RotatePiece(_pieceI180);

        RotatePiece(_pieceI270);
        RotatePiece(_pieceI270);
        RotatePiece(_pieceI270);

        PrintMatrix(_pieceI);
        PrintMatrix(_pieceI90);
        PrintMatrix(_pieceI180);
        PrintMatrix(_pieceI270);
    }

    private void PrintMatrix(int[][] matrix)
    {
        for (int i = 0; i < matrix.Length; i++)
        {
            for (int j = 0; j < matrix[i].Length; j++)
            {
                Console.Write(matrix[i][j]);
            }
            Console.WriteLine("");
        }

        Console.WriteLine("");
    }

    public override void Render(Batcher batcher, Camera camera)
    {
        // Pieces:
        for (int i = 0; i < _pieces.Count; i++)
        {
            var p = _pieces[i];
            var vals = p.GetAllPositions().Values.ToList();

            for (int j = 0; j < vals.Count; j++)
            {
                var points = vals[j];
                //RotatePoints(points, Point.Empty, 90);
                
                var basePos = new Vector2(128 * i + 128, 128 * j + 128);
                
                foreach (var pos in points)
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
                    //batcher.DrawString(_skin.DebugFont, $"{{0, 0}}", basePos + Vector2.One * 17, Color.White);
                }
                else
                {
                    batcher.DrawPixel(basePos, Color.LightBlue, 6);
                    //batcher.DrawString(_skin.DebugFont, $"{{0, 0}}", basePos + Vector2.One * 2, Color.White);
                }
            }
        }
        
        
        // Wall Kick Data
        /*var vals = PieceSettings.DefaultWallKickData.Values.ToList();
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
        }*/

        
        var rotations = new [] { 0, 90, 180, 270 };
        for (int j = 0; j < _pieces.Count; j++)
        {
            var piece = _pieces[j];
            for (int i = 0; i < rotations.Length; i++)
            {
                var rotation = rotations[i];
                var basePos = new Vector2(128 + j * 128, 800 + i * 128);

                foreach (var p in piece.CurrentPos)
                {
                    var r = GlobalTimeManager.TimeFunc(() => RotatePoint(p, Point.Empty, rotation), out _, "RotatePoint");
                    batcher.DrawHollowRect(
                        new Vector2(basePos.X + r.X * 32, basePos.Y + r.Y * 32),
                        32,
                        32,
                        Color.Red
                    );
                }

                if (piece.OffsetType == OffsetType.Cell)
                {
                    batcher.DrawPixel(basePos + Vector2.One * 16, Color.LightBlue, 6);
                    //batcher.DrawString(_skin.DebugFont, $"{{0, 0}}", basePos + Vector2.One * 17, Color.White);
                }
                else
                {
                    batcher.DrawPixel(basePos, Color.LightBlue, 6);
                    //batcher.DrawString(_skin.DebugFont, $"{{0, 0}}", basePos + Vector2.One * 2, Color.White);
                }
            }
        }

        /*var points = piece.CurrentPos;
        for (int i = 0; i < points.Length; i++)
        {
            var p = points[i];
            var r = RotatePoint(p, Point.Empty, 180);

            batcher.DrawHollowRect(
                new Vector2(128 + r.X * 32, 128 + r.Y * 32),
                32,
                32,
                Color.Red
            );
        }*/
    }

    private void RotatePiece(int[][] a)
    {
        GlobalTimeManager.TimeAction(() =>
        {
            int n = a.GetLength(0);
            int tmp;

            for (int i = 0; i < n / 2; i++)
            {
                for (int j = i; j < n - i - 1; j++)
                {
                    tmp = a[i][j];
                    a[i][j] = a[j][n - i - 1];
                    a[j][n - i - 1] = a[n - i - 1][n - j - 1];
                    a[n - i - 1][n - j - 1] = a[n - j - 1][i];
                    a[n - j - 1][i] = tmp;
                }
            }
        }, "RotatePiece");
    }
    
    static Point RotatePoint(Point pointToRotate, Point centerPoint, double angleInDegrees)
    {
        if (angleInDegrees == 0)
            return pointToRotate;
        
        double angleInRadians = angleInDegrees * (Math.PI / 180);
        double cosTheta = Math.Cos(angleInRadians);
        double sinTheta = Math.Sin(angleInRadians);
        return new Point
        {
            X =
                (int)
                Math.Round((cosTheta * (pointToRotate.X - centerPoint.X) -
                    sinTheta * (pointToRotate.Y - centerPoint.Y) + centerPoint.X)),
            Y =
                (int)
                Math.Round((sinTheta * (pointToRotate.X - centerPoint.X) +
                  cosTheta * (pointToRotate.Y - centerPoint.Y) + centerPoint.Y))
        };
    }

    static void RotatePoints(Point[] pointsToRotate, Point origin, double angleInDegrees)
    {
        for (int i = 0; i < pointsToRotate.Length; i++)
        {
            pointsToRotate[i] = RotatePoint(pointsToRotate[i], origin, angleInDegrees);
        }
    }
}