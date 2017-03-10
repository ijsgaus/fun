namespace Fun.Ast
{
    public abstract class LiteralValue
    {
        private LiteralValue()
        {
            
        }

        public class I8 : LiteralValue
        {
            public I8(sbyte value)
            {
                Value = value;
            }

            public sbyte Value { get; }
        }

        public class U8 : LiteralValue
        {
            public U8(byte value)
            {
                Value = value;
            }

            public byte Value { get; }
        }

        public class I16 : LiteralValue
        {
            public I16(short value)
            {
                Value = value;
            }

            public short Value { get; }
        }

        public class U16 : LiteralValue
        {
            public U16(ushort value)
            {
                Value = value;
            }

            public ushort Value { get; }
        }

        public class I32 : LiteralValue
        {
            public I32(int value)
            {
                Value = value;
            }

            public int Value { get; }
        }

        public class U32 : LiteralValue
        {
            
        }

    }
}