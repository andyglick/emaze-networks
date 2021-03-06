package net.emaze.networks.jackson;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import java.io.IOException;
import net.emaze.networks.ipv6.Ipv6Mask;

public class Ipv6MaskFromString extends JsonDeserializer<Ipv6Mask> {

    @Override
    public Ipv6Mask deserialize(JsonParser jp, DeserializationContext dc) throws IOException, JsonProcessingException {
        return Ipv6Mask.net(jp.getIntValue());
    }
}
