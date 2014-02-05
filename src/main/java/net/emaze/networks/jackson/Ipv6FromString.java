package net.emaze.networks.jackson;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import java.io.IOException;
import net.emaze.networks.Ipv6;

public class Ipv6FromString extends JsonDeserializer<Ipv6> {

    @Override
    public Ipv6 deserialize(JsonParser jp, DeserializationContext dc) throws IOException, JsonProcessingException {
        return Ipv6.parse(jp.getText());
    }
}
