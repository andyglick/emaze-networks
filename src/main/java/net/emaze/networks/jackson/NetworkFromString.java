package net.emaze.networks.jackson;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import java.io.IOException;
import net.emaze.networks.Network;

public class NetworkFromString extends JsonDeserializer<Network> {

    @Override
    public Network deserialize(JsonParser jp, DeserializationContext dc) throws IOException, JsonProcessingException {
        return Network.fromCidrNotation(jp.getText());
    }
}
