using System;
using System.Collections.Generic;
using System.Linq;

namespace Quader.Engine
{
    public class TimeData
    {
        public string MethodName { get; set; }
        public TimeSpan LastTime { get; set; }
        public TimeSpan MeanTime { get; set; }

        public List<TimeSpan> AllTimes { get; } = new ();

        public override string ToString()
        {
            return $"{MethodName}:    Last: {LastTime.TotalMilliseconds} | Mean: {AllTimes.Select(span => span.TotalMilliseconds).Average():F6}";
        }
    }
    
    public static class GlobalTimeManager
    {
        public static Dictionary<string, TimeData> TimeData { get; } = new ();

        public static void AddData(string methodName, TimeSpan lastTime)
        {
            if (!TimeData.ContainsKey(methodName))
            {
                TimeData[methodName] = new TimeData
                {
                    MethodName = methodName,
                    LastTime = lastTime
                };
                
                TimeData[methodName].AllTimes.Add(lastTime);
            }
            else
            {
                TimeData[methodName].LastTime = lastTime;
                TimeData[methodName].AllTimes.Add(lastTime);
            }
        }
    }
}