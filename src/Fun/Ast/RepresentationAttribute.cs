using System;
using System.Reflection;

namespace Fun.Ast
{
    [AttributeUsage(AttributeTargets.All)]
    public class RepresentationAttribute : Attribute
    {
        public RepresentationAttribute(string data)
        {
            Data = data;
        }

        public string Data { get; }

        public static string GetData(ICustomAttributeProvider cap)
        {
            var attr = cap.GetCustomAttribute<RepresentationAttribute>();
            return attr?.Data;
        }
    }
}