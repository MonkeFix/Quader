using System;

namespace Quader.Engine;

[AttributeUsage(AttributeTargets.Field)]
public sealed class MoveTypeAttribute : Attribute
{
    public string Description { get; set; }

    public MoveTypeAttribute(string description = "")
    {
        Description = description;
    }
}