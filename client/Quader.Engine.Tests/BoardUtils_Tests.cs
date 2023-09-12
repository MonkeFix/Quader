using Microsoft.Xna.Framework;
using NUnit.Framework;

namespace Quader.Engine.Tests;

[TestFixture]
public class BoardUtils_Tests
{
    [Test]
    public void AdjustPositions_Test()
    {
        TestOnePos(new []
        {
            new Point(0, 0),
            new Point(1, 0),
            new Point(0, 1),
            new Point(1, 1)
        }, new Point(1, 2));
    }

    private void TestOnePos(Point[] data, Point offset)
    {
        var length = data.Length;

        var adjusted = BoardUtils.AdjustPositions(data, offset);

        for (int i = 0; i < length; i++)
        {
            var a = data[i] + offset;
            var b = adjusted[i];

            Assert.AreEqual(a.X, b.X);
            Assert.AreEqual(a.Y, b.Y);
        }
    }
}