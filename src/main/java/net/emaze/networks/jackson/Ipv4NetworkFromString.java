package net.emaze.networks.jackson;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import java.io.IOException;
import net.emaze.networks.ipv4.Ipv4Network;

public class Ipv4NetworkFromString extends JsonDeserializer<Ipv4Network> {

    @Override
    public Ipv4Network deserialize(JsonParser jp, DeserializationContext dc) throws IOException, JsonProcessingException {
        return Ipv4Network.fromCidrNotation(jp.getText());
    }
}
