using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Newtonsoft.Json;
using Nez;
using Quader.Engine;

namespace Quader
{
    public enum PieceStartPosition
    {
        Initial,
        RotationClockwise,
        RotationCounterClockwise,
        Rotation180Deg
    }

    public class RotationEncoding
    {
        public PieceStartPosition StartPosition { get; set; }
        public string[] InitialEncoding { get; set; } = null!;
        public string[][] TestsClockwise { get; set; } = null!;
        public string[][] TestsCounterClockwise { get; set; } = null!;
    }

    public class RotationPositionEncoding
    {
        public Dictionary<PieceStartPosition, RotationEncoding> PositionEncodings { get; set; } = null!;
    }

    public class RotationSystemTable
    {
        [JsonProperty(PropertyName = "RotationSystemTable")]
        public Dictionary<PieceType, RotationPositionEncoding> RotationSystemTableMap { get; set; } = null!;
    }

    public class ConverterOptions
    {
        public int SegmentSize { get; set; }
        public int XOffset { get; set; }
        public int YOffset { get; set; }
        public int TestCount { get; set; }

        public static ConverterOptions Default => new ConverterOptions
        {
            SegmentSize = 16,
            XOffset = 8,
            YOffset = 8,
            TestCount = 5
        };
    }

    public static class RotationImageToJsonConverter
    {
        public static string ConvertToJsonDebug(Texture2D image, out TimeSpan timeSpent)
        {
            string res = "";
            timeSpent = Debug.TimeAction(() => { res = ConvertToJson(image); });



            return res;
        }

        public static string ConvertToJson(Texture2D image, ConverterOptions? options = null)
        {
            Color[] data = new Color[image.Width * image.Height];

            image.GetData(data);
            
            if (options == null)
                options = ConverterOptions.Default;

            var result = "";
            var segmentSize = options.SegmentSize;
            var xOffset = options.XOffset;
            var yOffset = options.YOffset;

            Color[,] newData = new Color[image.Height, image.Width];

            // Convert 1D array to 2D array
            for (int y = 0; y < image.Height; y++)
            {
                for (int x = 0; x < image.Width; x++)
                {
                    var index = x + image.Width * y;

                    newData[y, x] = data[index];
                }
            }

            var rst = new RotationSystemTable();
            rst.RotationSystemTableMap = new Dictionary<PieceType, RotationPositionEncoding>();

            // Step 1: Take the entire piece table from the image
            Dictionary<PieceType, Color[,]> piecesDataRaw = new Dictionary<PieceType, Color[,]>(7);

            for (int i = 0; i < 7; i++)
            {
                var startX = 0;
                var startY = i * (segmentSize * 4); // 4 rows

                var width = image.Width;
                var height = segmentSize * 4; // 4 rows

                var portion = TakePortion(newData, startX, startY, width, height, new Color(1.0f, 0, 0, 1.0f));
                var pieceType = (PieceType)i; // I, O, T, L, J, S, Z
                piecesDataRaw[pieceType] = portion;

                // Step 2: Split piece table to rows
                Dictionary<PieceStartPosition, Color[,]> piecesRowsRaw = new Dictionary<PieceStartPosition, Color[,]>(4);
                var dataRawArr = piecesDataRaw.Values.ToArray();

                // Proceed every row
                for (int j = 0; j < 4; j++)
                {
                    startX = 0;
                    startY = j * segmentSize;

                    width = image.Width;
                    height = segmentSize;

                    portion = TakePortion(dataRawArr[i], startX, startY, width, height);
                    var pieceStartPos = (PieceStartPosition)j;
                    piecesRowsRaw[pieceStartPos] = portion;

                    // Step 3: Create 4 sets of tests for both clockwise and counter-clockwise rotation and 4 types of initial position
                    // 0       - initial position
                    // [1,  5] - clockwise tests
                    // [6, 10] - counter-clockwise tests
                    var initialPosData = TakePortion(dataRawArr[i], 0, 0, segmentSize, segmentSize, Color.White);

                    List<Color[,]> clockwiseTestData = new List<Color[,]>(5);
                    for (int k = 1; k <= options.TestCount; k++)
                    {
                        clockwiseTestData.Add(TakePortion(dataRawArr[i], segmentSize * k, 0, segmentSize, segmentSize));
                    }

                    List<Color[,]> counterClockwiseTestData = new List<Color[,]>(5);
                    for (int k = options.TestCount + 1; k <= options.TestCount * 2; k++)
                    {
                        counterClockwiseTestData.Add(TakePortion(dataRawArr[i], segmentSize * k, 0, segmentSize, segmentSize));
                    }

                    var re = new RotationEncoding
                    {
                        StartPosition = pieceStartPos,
                        InitialEncoding = Encode(initialPosData),
                        TestsClockwise = Encode(clockwiseTestData.ToArray()),
                        TestsCounterClockwise = Encode(counterClockwiseTestData.ToArray())
                    };

                    var rpe = new RotationPositionEncoding
                    {
                        PositionEncodings = new Dictionary<PieceStartPosition, RotationEncoding>
                        {
                            [pieceStartPos] = re
                        }
                    };

                    rst.RotationSystemTableMap[pieceType] = rpe;
                }
            }

            result = JsonConvert.SerializeObject(rst, Formatting.Indented);

            using (var sw = new StreamWriter("data.json", false))
            {
                sw.WriteLine(result);
            }

            return result;
        }

        private static string[] Encode(Color[,] data)
        {
            var result = new string[data.GetLength(1)];

            for (int y = 0; y < data.GetLength(1); y++)
            {
                var curStr = "";
                for (int x = 0; x < data.GetLength(0); x++)
                {
                    var d = data[y, x];
                    if (d == Color.Black)
                        curStr += "X";
                    else
                        curStr += ".";

                }

                result[y] = curStr;
            }

            return result;
        }

        private static string[][] Encode(Color[][,] data)
        {
            var result = new List<string[]>();

            for (int i = 0; i < data.Length; i++)
            {
                var encoded = Encode(data[i]);
                result.Add(encoded);
            }

            return result.ToArray();
        }

        private static Color[,] TakePortion(Color[,] data, int x, int y, int width, int height, Color? colorToIgnore = null)
        {
            var result = new Color[height, width];

            for (int i = y; i < y + height; i++)
            {
                for (int j = x; j < x + width; j++)
                {
                    if (colorToIgnore != null && data[i, j] == colorToIgnore.Value)
                    {
                        result[i - y, j - x] = Color.Transparent;
                    }
                    else
                    {
                        result[i - y, j - x] = data[i, j];
                    }
                }
            }

            return result;
        }
    }
}