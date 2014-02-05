package net.emaze.networks.jackson;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import java.io.IOException;
import net.emaze.networks.Ipv6Network;

public class Ipv6NetworkFromString extends JsonDeserializer<Ipv6Network> {

    @Override
    public Ipv6Network deserialize(JsonParser jp, DeserializationContext dc) throws IOException, JsonProcessingException {
        return Ipv6Network.fromCidrNotation(jp.getText());
    }
}
