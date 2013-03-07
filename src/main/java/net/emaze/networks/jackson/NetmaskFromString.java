package net.emaze.networks.jackson;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import java.io.IOException;
import net.emaze.networks.Netmask;

public class NetmaskFromString extends JsonDeserializer<Netmask> {

    @Override
    public Netmask deserialize(JsonParser jp, DeserializationContext dc) throws IOException, JsonProcessingException {
        return Netmask.fromBits(jp.getIntValue());
    }
}
