using System;
using System.Collections;
using Microsoft.Xna.Framework;
using Nez.Persistence;

namespace Quader.Serialization;

public class ColorJsonConverter : JsonTypeConverter<Color>
{
    public override bool WantsExclusiveWrite => true;

    public override void WriteJson(IJsonEncoder encoder, Color value)
    {
        var color = (Color)value;

        encoder.EncodeKeyValuePair("R", color.R);
        encoder.EncodeKeyValuePair("G", color.G);
        encoder.EncodeKeyValuePair("B", color.B);
        encoder.EncodeKeyValuePair("A", color.A);
    }

    public override void OnFoundCustomData(Color instance, string key, object value)
    { }
}

public class ColorFactoryConverter : JsonObjectFactory<Color>
{
    public override Color Create(Type objectType, IDictionary objectData)
    {
        var color = new Color();

        color.R = Convert.ToByte(objectData["R"]);
        color.G = Convert.ToByte(objectData["G"]);
        color.B = Convert.ToByte(objectData["B"]);
        color.A = Convert.ToByte(objectData["A"]);

        return color;
    }
}